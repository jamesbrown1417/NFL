from playwright.async_api import async_playwright
import asyncio

# The URL pattern we are interested in
<<<<<<< HEAD
url_pattern = "https://api.neds.com.au/v2/sport/event-request?category_ids=%5B%2271955b54-62f6-4ac5-abaa-df88cad0aeef%22%5D&include_any_team_vs_any_team_events=true"
=======
url_pattern = "https://api.neds.com.au/v2/sport/event-request?category_ids=%5B%22a19fe930-3d0c-4f23-9cd4-12132fcc6b0a%22%5D&include_any_team_vs_any_team_events=true"
>>>>>>> a6f079d44f07a234764e959ac4df4744d4573c6b

async def close_browser(browser):
    """Close the browser."""
    await browser.close()
    print("Browser closed.")

async def main():
    async with async_playwright() as p:
        # Launch the browser
        browser = await p.chromium.launch()
        page = await browser.new_page()

        # Listen for the specific network response
        async def handle_response(response):
            if response.url == url_pattern:
                # Fetch and decode the response body
                body = await response.body()
                # Write out the body of the response to a file
<<<<<<< HEAD
                with open("OddsScraper\\EPL\\Neds\\neds_response.json", "w", encoding = 'utf-8') as f:
=======
                with open("OddsScraper\\Neds\\neds_response.json", "w", encoding = 'utf-8') as f:
>>>>>>> a6f079d44f07a234764e959ac4df4744d4573c6b
                    f.write(body.decode("utf-8"))
                # Print a message to the console
                print("Response captured!")
                # Schedule browser to close
                asyncio.create_task(close_browser(browser))

        # Attach the handler to the 'response' event
        page.on('response', handle_response)

        # Navigate to the target page
<<<<<<< HEAD
        await page.goto("https://www.neds.com.au/sports/soccer/uk-ireland/premier-league", wait_until="networkidle")
=======
        await page.goto("https://www.neds.com.au/sports/american-football/nfl/", wait_until="networkidle")
>>>>>>> a6f079d44f07a234764e959ac4df4744d4573c6b

        # Keep the script running until the browser is closed
        while len(await browser.contexts()) > 0:
            await asyncio.sleep(0.1)

# Run the script
asyncio.run(main())

