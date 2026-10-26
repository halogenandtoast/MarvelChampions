import { expect, test } from '@playwright/test'

for (const { code, name, ratio } of [
  { code: '12001', name: 'Ant-Man', ratio: 88 / 126 },
  { code: '13001', name: 'Wasp', ratio: 88 / 126 },
  { code: '42001', name: 'Archangel', ratio: 126 / 88 },
]) {
  test(`${name} unfolds at its printed orientation from either cover`, async ({ page }) => {
    await page.goto(`/#/cards/${code}a`)
    const frame = page.locator('.folding-frame')
    await expect(frame).toBeVisible()
    const tabs = page.getByRole('tab')
    const opened = page.getByRole('tab', { name: name === 'Archangel' ? 'Archangel' : 'Giant', exact: true })
    const hero = page.getByRole('tab', { name: name === 'Archangel' ? 'Angel' : 'Tiny', exact: true })
    const alterEgo = tabs.filter({ hasText: /Scott Lang|Nadia Van Dyne|Warren Worthington III/ })
    // Open from A, close to B, then reopen; both halves must remain in the frame.
    for (const tab of [opened, alterEgo, opened, hero, opened]) {
      await tab.click()
      await page.waitForTimeout(1150)
      if (tab !== opened) continue
      const box = (await frame.boundingBox())!
      expect(box.width / box.height).toBeCloseTo(ratio, 2)
      for (const half of await frame.locator('.inside .half').all()) {
        const bounds = (await half.boundingBox())!
        expect(bounds.x).toBeGreaterThanOrEqual(box.x - 1)
        expect(bounds.y).toBeGreaterThanOrEqual(box.y - 1)
        expect(bounds.x + bounds.width).toBeLessThanOrEqual(box.x + box.width + 1)
        expect(bounds.y + bounds.height).toBeLessThanOrEqual(box.y + box.height + 1)
      }
    }
    // The same face can also be linked directly, outside the animated identity.
    await page.goto(`/#/cards/${code}c`)
    const image = page.locator('.art > .frame')
    await expect(image).toBeVisible()
    const box = (await image.boundingBox())!
    expect(box.width / box.height).toBeCloseTo(ratio, 2)
  })
}

test('Archangel uses the full horizontal spread on the board, health meter and preview', async ({ page }) => {
  await page.goto('/#/cards/42001a')
  await expect(page.locator('.folding-frame')).toBeVisible()
  // Mount the real board components with a reactive player, without changing a saved game.
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h, reactive } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: PlayerArea } = await load('/src/components/PlayerArea.vue')
    const { default: CardOverlay } = await load('/src/components/CardOverlay.vue')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const player = reactive({
      id: 'folding-test', name: 'Angel', form: 'hero', code: '42001a',
      faces: ['42001a', '42001b', '42001c'],
      hitPoints: 12, damage: 0, exhausted: false, handSize: 5,
      thwart: 1, attack: 2, defense: 2, recover: 3, allies: [],
      supports: [], upgrades: [], discard: [], deckSize: 30,
    })
    const host = document.createElement('div')
    host.id = 'folding-board-test'
    document.body.replaceChildren(host)
    createApp({ setup: () => () => [h(PlayerArea, { player }), h(CardOverlay)] })
      .use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } }))
      .mount(host)
    Object.assign(window, { foldingPlayer: player })
  })
  const card = page.locator('.identity .card')
  const folded = (await card.boundingBox())!
  for (const cover of ['42001a', '42001b']) {
    await page.evaluate((code) => { (window as any).foldingPlayer.code = code }, cover)
    await page.waitForTimeout(1150)
    await page.evaluate(() => { (window as any).foldingPlayer.code = '42001c' })
    await page.waitForTimeout(1150)
    const open = (await card.boundingBox())!
    expect(open.width).toBeCloseTo(folded.width * 2, 0)
    expect(open.height).toBeCloseTo(folded.height, 0)
    expect((await page.locator('.identity > .meter').boundingBox())!.width).toBeCloseTo(open.width, 0)
  }
  await card.hover()
  const preview = page.locator('.card-overlay')
  await expect(preview).toBeVisible()
  const box = (await preview.boundingBox())!
  expect(box.width / box.height).toBeCloseTo(126 / 88, 2)
  await page.evaluate(() => { (window as any).foldingPlayer.exhausted = true })
  await page.waitForTimeout(300)
  const exhausted = (await card.boundingBox())!
  expect(exhausted.width).toBeCloseTo(folded.height * 0.9, 0)
  expect(exhausted.height).toBeCloseTo(folded.width * 2 * 0.9, 0)
  expect((await page.locator('.identity > .meter').boundingBox())!.y).toBeGreaterThan(exhausted.y + exhausted.height)
})
