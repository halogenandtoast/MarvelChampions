import { expect, test, type Page } from '@playwright/test'

async function mount(page: Page) {
  await page.goto('/#/cards/01001a')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h, ref } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: CampaignCards } = await load('/src/components/CampaignCards.vue')
    const { default: CardOverlay } = await load('/src/components/CardOverlay.vue')
    const { useCards } = await load('/src/cards.ts')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    await useCards().store.load()
    const shown = ref(true)
    const host = document.createElement('div')
    host.style.cssText = 'padding: 80px 20px; max-width: 600px'
    document.body.replaceChildren(host)
    createApp({ setup: () => () => [
      shown.value && h(CampaignCards, { codes: ['04159b', '04098', '04099', '01107'] }),
      h('div', { id: 'empty' }, h(CampaignCards, { codes: [null, undefined] })),
      h(CardOverlay),
    ] }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
    Object.assign(window, { removeEarnedCards: () => { shown.value = false } })
  })
}

test('earned cards preview individually on hover and keyboard focus, including landscape cards', async ({ page }) => {
  await mount(page)
  const overlay = page.locator('.card-overlay')
  for (const code of ['04159b', '04098', '04099', '01107']) {
    const reference = page.locator(`[data-card-code="${code}"]`)
    await reference.hover()
    await expect(overlay).toBeVisible()
    await expect(overlay.locator('.face img')).toHaveAttribute('src', new RegExp(`${code}\\.avif`))
    expect(await overlay.evaluate(el => el.getBoundingClientRect().width > el.getBoundingClientRect().height)).toBe(code === '01107')
    await page.mouse.move(0, 0)
    await expect(overlay).toHaveCount(0)
  }
  await page.locator('[data-card-code="04098"]').focus()
  await expect(overlay).toBeVisible()
  await page.keyboard.press('Escape')
  await expect(overlay).toHaveCount(0)
  await page.locator('[data-card-code="04099"]').focus()
  await expect(overlay).toBeVisible()
  await page.evaluate(() => (window as any).removeEarnedCards())
  await expect(overlay).toHaveCount(0)
  await expect(page.locator('#empty')).toHaveText('—')
  await expect(page.locator('#empty button')).toHaveCount(0)
})

test.describe('touch', () => {
  test.use({ viewport: { width: 390, height: 844 }, hasTouch: true, isMobile: true })
  test('tapping an earned card pins its preview until the next tap', async ({ page }) => {
    await mount(page)
    await page.locator('[data-card-code="04098"]').tap()
    const overlay = page.locator('.card-overlay')
    await expect(overlay).toBeVisible()
    await expect(overlay.locator('.face img')).toHaveAttribute('src', /04098\.avif/)
    const bounds = (await overlay.boundingBox())!
    expect(bounds.x).toBeGreaterThanOrEqual(0)
    expect(bounds.x + bounds.width).toBeLessThanOrEqual(390)
    await page.touchscreen.tap(5, 5)
    await expect(overlay).toHaveCount(0)
  })
})
