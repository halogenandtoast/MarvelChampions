import { expect, test } from '@playwright/test'
import { gzipSync } from 'node:zlib'

const preview = { id: 'original', name: 'Friday Rhino', playerName: 'Matt', campaignId: null,
  scenario: 'Rhino', villain: '01094', difficulty: 'Expert', round: 7, phase: 'PlayerPhase',
  outcome: null, waiting: false, seats: [{ name: 'Matt', identity: '01019a', damage: 3, eliminated: false }] }
const saved = { version: 1, name: preview.name, game: {}, queue: [], steps: [], preview }

test.beforeEach(async ({ page }) => {
  const user = { id: 123, username: 'fixture', email: '', admin: false, beta: true, guest: false }
  await page.addInitScript(user => localStorage.setItem('marvel-session', JSON.stringify({ token: 'fixture', user })), user)
  await page.route('**/api/whoami', route => route.fulfill({ json: user }))
  for (const path of ['games', 'campaigns', 'spectatable']) await page.route(`**/api/${path}`, route => route.fulfill({ json: [] }))
  await page.goto('/#/')
  await page.getByRole('button', { name: 'Load game', exact: true }).click()
})

for (const compressed of [false, true]) {
  test(`previews ${compressed ? 'gzip' : 'JSON'} locally and uploads the original bytes`, async ({ page }) => {
    const bytes = Buffer.from(JSON.stringify(saved))
    const buffer = compressed ? gzipSync(bytes) : bytes
    let uploaded = false
    await page.route('**/api/import', async route => {
      uploaded = true
      expect(route.request().postDataBuffer()).toEqual(buffer)
      await route.fulfill({ status: 400, body: '$error.invalidGameFile' })
    })
    await page.locator('input[type=file]').setInputFiles({ name: compressed ? 'save.json.gz' : 'save.json', mimeType: 'application/octet-stream', buffer })
    const row = page.locator('.load-game .game-row')
    for (const text of ['Friday Rhino', 'Rhino', 'Expert', '7', 'Matt']) await expect(row).toContainText(text)
    await expect(row.locator('a')).toHaveCount(0)
    expect(uploaded).toBe(false)
    await page.locator('.load-actions').getByRole('button', { name: 'Load game', exact: true }).click()
    await expect(page.locator('.import-error')).toBeVisible()
    expect(uploaded).toBe(true)
  })
}

test('legacy saves remain loadable without preview metadata', async ({ page }) => {
  const { preview: _, ...legacy } = saved
  await page.locator('input[type=file]').setInputFiles({ name: 'legacy.json', mimeType: 'application/json', buffer: Buffer.from(JSON.stringify(legacy)) })
  await expect(page.locator('.load-game')).toContainText('Friday Rhino')
  await expect(page.locator('.load-actions').getByRole('button', { name: 'Load game', exact: true })).toBeEnabled()
})

test('a corrupt gzip clears the previous preview and disables import', async ({ page }) => {
  const input = page.locator('input[type=file]')
  await input.setInputFiles({ name: 'good.gz', mimeType: 'application/gzip', buffer: gzipSync(JSON.stringify(saved)) })
  await expect(page.locator('.load-game .game-row')).toBeVisible()
  await input.setInputFiles({ name: 'bad.gz', mimeType: 'application/gzip', buffer: Buffer.from([0x1f, 0x8b, 0]) })
  await expect(page.locator('.import-error')).toBeVisible()
  await expect(page.locator('.load-game .game-row')).toHaveCount(0)
  await expect(page.locator('.load-actions').getByRole('button', { name: 'Load game', exact: true })).toBeDisabled()
})
