import { defineConfig, devices } from '@playwright/test'

// The tests drive the dev stack: Vite here, proxying /api to the game server on API_PORT,
// which this config does not start (it needs stack and Postgres), so start it first.
export default defineConfig({
  testDir: './e2e',
  fullyParallel: true,
  reporter: process.env.CI ? 'github' : 'list',
  use: {
    baseURL: 'http://localhost:5180',
    trace: 'retain-on-failure',
  },
  projects: [{ name: 'chromium', use: { ...devices['Desktop Chrome'], viewport: { width: 1600, height: 1000 } } }],
  webServer: {
    command: "npx vite --port 5180 --strictPort",
    url: 'http://localhost:5180',
    reuseExistingServer: true,
  },
})
