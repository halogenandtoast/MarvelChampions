import { expect, test } from '@playwright/test'

test('Steady status cards remain readable and duplicates overlap with aligned top edges', async ({ page }) => {
  await page.goto('/#/cards/45030a')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h, reactive } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: BoardCard } = await load('/src/components/BoardCard.vue')
    const { default: CardOverlay } = await load('/src/components/CardOverlay.vue')
    const { useCards } = await load('/src/cards.ts')
    await useCards().store.load()
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const status = reactive({ stunCards: 1, confuseCards: 1, stunned: false, confused: false, tough: false, toughCards: 0 })
    const host = document.createElement('div')
    host.style.cssText = '--card-width: 120px; padding: 120px'
    document.body.replaceChildren(host)
    createApp({ setup: () => () => [h(BoardCard, { id: 'magik', code: '45030a', ...status }), h(CardOverlay)] })
      .use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
    Object.assign(window, { steadyStatus: status })
  })
  const card = page.locator('[data-card-id="magik"]')
  await expect(card.getByAltText('Confused (inactive)', { exact: true })).toBeVisible()
  await expect(card.locator('.statuses img.inactive')).toHaveCount(2)
  expect(await card.locator('.statuses img').first().evaluate(el => getComputedStyle(el).opacity)).toBe('1')
  await card.hover()
  await expect(page.locator('.card-overlay .statuses img.inactive')).toHaveCount(2)
  await page.mouse.move(0, 0)
  await page.evaluate(() => Object.assign((window as any).steadyStatus, { confuseCards: 2, confused: true }))
  const confused = card.getByAltText('Confused', { exact: true })
  await expect(confused).toHaveCount(2)
  const first = (await confused.nth(0).boundingBox())!
  const second = (await confused.nth(1).boundingBox())!
  expect(second.y).toBe(first.y)
  expect(second.x).toBeGreaterThan(first.x)
  expect(second.x).toBeLessThan(first.x + first.width)
  await expect(card.locator('.statuses img.inactive')).toHaveCount(1)
  await card.hover()
  await expect(page.locator('.card-overlay').getByAltText('Confused', { exact: true })).toHaveCount(2)
  await page.mouse.move(0, 0)
  await page.evaluate(() => Object.assign((window as any).steadyStatus, { stunCards: 2, stunned: true, toughCards: 2, tough: true }))
  await expect(card.locator('.status-group')).toHaveCount(3)
  await expect(card.locator('.statuses img')).toHaveCount(6)
  const board = (await card.boundingBox())!
  const statuses = (await card.locator('.statuses').boundingBox())!
  expect(statuses.y + statuses.height).toBeLessThan(board.y + board.height * 0.6)
  for (const group of await card.locator('.status-group').all()) {
    const a = (await group.locator('img').nth(0).boundingBox())!
    const b = (await group.locator('img').nth(1).boundingBox())!
    expect(b.y).toBe(a.y)
    expect(b.x).toBeGreaterThan(a.x)
    expect(b.x).toBeLessThan(a.x + a.width)
  }
  await card.hover()
  const preview = page.locator('.card-overlay .statuses')
  await expect(preview.locator('.status-group')).toHaveCount(3)
  await expect(preview.locator('img')).toHaveCount(6)
  expect((await preview.boundingBox())!.height).toBeLessThan((await page.locator('.card-overlay').boundingBox())!.height)

})
