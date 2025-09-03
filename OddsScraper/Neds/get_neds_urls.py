from playwright.async_api import async_playwright
import asyncio
import pathlib
import logging

# --- Configuration ---
# Page to navigate to (NFL)
NAVIGATE_URL = "https://www.neds.com.au/sports/american-football/nfl/"
# Target API base and category ID (match flexibly; params may vary/order)
TARGET_API_BASE = "https://api.neds.com.au/v2/sport/event-request"
NFL_CATEGORY_ID = "a19fe930-3d0c-4f23-9cd4-12132fcc6b0a"
# Output file path using pathlib
OUTPUT_FILE = pathlib.Path("OddsScraper/Neds/neds_response.json")
# Timeouts (in milliseconds for Playwright, seconds for asyncio.wait_for)
PAGE_LOAD_TIMEOUT = 45000  # 45 seconds
WAIT_FOR_RESPONSE_TIMEOUT = 60  # 60 seconds for waiting the specific response

# Configure basic logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
# --- End Configuration ---

async def main():
    """
    Launches a browser, navigates to a page, intercepts a specific network response,
    saves its body to a file, and then cleans up.
    """
    # Future to signal when the target response has been captured and processed
    response_processed_future = asyncio.Future()

    async with async_playwright() as p:
        browser = None  # Initialize browser variable
        try:
            logging.info("Launching browser...")
            browser = await p.chromium.launch()
            # Use context manager for the browser instance to ensure it's closed
            async with browser:
                page = await browser.new_page()
                logging.info("New page created.")

                # --- Define the response handler ---
                async def handle_response(response):
                    # Check if we already processed the response or if the future is done
                    if response_processed_future.done():
                        return

                    url = response.url
                    # Relaxed match: base path + contains NFL category id
                    if url.startswith(TARGET_API_BASE) and NFL_CATEGORY_ID in url:
                        logging.info(f"Target response received from: {url}")
                        try:
                            body = await response.body()  # Get raw bytes
                            # Ensure the output directory exists
                            OUTPUT_FILE.parent.mkdir(parents=True, exist_ok=True)
                            # Write the decoded body to the file
                            with open(OUTPUT_FILE, "w", encoding="utf-8") as f:
                                f.write(body.decode("utf-8"))
                            logging.info(f"Response body successfully saved to {OUTPUT_FILE}")
                            # Signal that we're done by setting the future's result
                            response_processed_future.set_result(True)
                        except Exception as e:
                            logging.error(f"Error processing response or writing file: {e}")
                            # Signal failure by setting an exception on the future
                            if not response_processed_future.done():
                                response_processed_future.set_exception(e)
                # --- End response handler definition ---

                # Attach the handler to the 'response' event
                page.on('response', handle_response)
                logging.info("Response handler attached.")

                logging.info(f"Navigating to {NAVIGATE_URL}...")
                try:
                    # Navigate to the target page; use domcontentloaded to avoid hanging on live polls
                    await page.goto(NAVIGATE_URL, wait_until="domcontentloaded", timeout=PAGE_LOAD_TIMEOUT)
                    logging.info("Navigation complete. Waiting for target network response...")
                except Exception as e:
                    logging.error(f"Error during navigation or page load: {e}")
                    # If navigation fails, signal failure immediately
                    if not response_processed_future.done():
                        response_processed_future.set_exception(e)
                    # No point waiting further if navigation failed
                    raise  # Re-raise the exception to stop execution here

                # Wait for the response handler to signal completion (or timeout)
                try:
                    await asyncio.wait_for(response_processed_future, timeout=WAIT_FOR_RESPONSE_TIMEOUT)
                    logging.info("Target response captured and processed successfully.")
                except asyncio.TimeoutError:
                    logging.error(
                        f"Timeout: Did not receive a target response from '{TARGET_API_BASE}' (category {NFL_CATEGORY_ID}) within {WAIT_FOR_RESPONSE_TIMEOUT} seconds."
                    )
                except Exception as e:
                    # This catches exceptions set on the future (e.g., file writing error)
                    logging.error(f"An error occurred during response processing: {e}")

            # Browser context automatically closes here (due to 'async with browser:')
            logging.info("Browser context closed.")

        except Exception as e:
            logging.error(f"An unexpected error occurred in main execution: {e}")
        finally:
            # Ensure browser is closed if launched but context manager failed somehow (belt-and-braces)
            if browser and browser.is_connected():
                logging.warning("Forcing browser close in finally block.")
                await browser.close()

        logging.info("Successfully finished!")

# Run the script using the standard Python entry point guard
if __name__ == "__main__":
    asyncio.run(main())
