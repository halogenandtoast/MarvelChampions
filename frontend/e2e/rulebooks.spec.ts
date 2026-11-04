import { expect, test } from '@playwright/test'

test('every cached official rulebook is served locally as a PDF', async ({ request }) => {
  const manifest = await (await request.get('/rulebooks/sources.json')).json()
  expect(Object.keys(manifest).length).toBeGreaterThanOrEqual(57)
  for (const [filename, entry] of Object.entries(manifest) as [string, { bytes: number; sha256: string }][]) {
    const response = await request.head(`/rulebooks/${filename}`)
    expect(response.status(), filename).toBe(200)
    expect(response.headers()['content-type'], filename).toContain('application/pdf')
    expect(Number(response.headers()['content-length']), filename).toBe(entry.bytes)
    expect(entry.sha256, filename).toMatch(/^[a-f0-9]{64}$/)
  }
  const response = await request.get('/rulebooks/mc55_rulebook-web.pdf', { headers: { Range: 'bytes=0-4' } })
  expect(response.status()).toBe(206)
  expect((await response.body()).toString()).toBe('%PDF-')
})

test('card source links open the local official guide', async ({ page }) => {
  await page.goto('/#/cards/60065')
  await expect(page.getByRole('link', { name: /View source/ })).toHaveAttribute('href', '/rulebooks/mc60_rulebook-web.pdf')
  const mapped = await page.evaluate(async () => {
    const { localRulebookUrl } = await import(/* @vite-ignore */ '/src/rulebooks.ts')
    return [
      localRulebookUrl('https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc60_rulebook-web.pdf#page=9'),
      localRulebookUrl('https://marvelcdb.com/card/01001a'),
    ]
  })
  expect(mapped).toEqual(['/rulebooks/mc60_rulebook-web.pdf#page=9', 'https://marvelcdb.com/card/01001a'])
})

test('scenario setup links to the locally served product rulebook', async ({ page }) => {
  const user = { id: 123, username: 'fixture', email: 'fixture@example.test', admin: false, beta: true, guest: false, allowSpectatorsByDefault: false }
  await page.addInitScript(user => localStorage.setItem('marvel-session', JSON.stringify({ token: 'fixture', user })), user)
  await page.route('**/api/whoami', route => route.fulfill({ json: user }))
  await page.route('**/api/scenarios', route => route.fulfill({ json: {
    scenarios: [{ code: '60121a', name: 'Art Museum Heist', modulars: [] }, { code: '60128a', name: 'The Getaway', modulars: [] }], modulars: [],
  } }))
  await page.goto('/#/new-game')
  await page.getByRole('radio').filter({ hasText: 'Art Museum Heist' }).click()
  await expect(page.getByRole('link', { name: 'Fear No Evil rulebook', exact: true })).toHaveAttribute('href', '/rulebooks/mc60_rulebook-web.pdf')
})
