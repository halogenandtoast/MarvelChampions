import { expect, test } from '@playwright/test'

test('preview closes when its card disappears without pointerleave', async ({ page }) => {
  await page.goto('/#/cards/01001a')
  const visible = await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { useCardOverlay } = await load('/src/composables/useCardOverlay.ts')
    const overlay = useCardOverlay()
    const container = document.createElement('div')
    const anchor = document.createElement('div')
    anchor.style.cssText = 'position:fixed;left:400px;top:300px;width:100px;height:140px'
    container.append(anchor)
    document.body.append(container)
    overlay.show({ code: '01001a' }, anchor)
    return overlay.visible.value
  })
  expect(visible).toBe(true)
  // Remove its entire zone, without moving the pointer or scrolling.
  await page.evaluate(() => document.querySelector('div[style*="left: 400px"]')!.parentElement!.remove())
  await expect.poll(() => page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { useCardOverlay } = await load('/src/composables/useCardOverlay.ts')
    return useCardOverlay().visible.value
  })).toBe(false)
})

for (const [code, landscape] of [['01107', true], ['01102', false]] as const) {
  test(`discard pile preview uses the printed orientation of ${code}`, async ({ page }) => {
    await page.goto('/#/cards/01001a')
    await page.evaluate(async code => {
      const load = (path: string) => import(/* @vite-ignore */ path)
      const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
      const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
      const { default: Pile } = await load('/src/components/Pile.vue')
      const { default: CardOverlay } = await load('/src/components/CardOverlay.vue')
      const { useCards } = await load('/src/cards.ts')
      await useCards().store.load()
      const { loadLocaleMessages } = await load('/src/locales/messages.ts')
      const { messages } = await loadLocaleMessages('en')
      const host = document.createElement('div')
      host.style.cssText = '--card-width: 100px; padding: 150px'
      document.body.replaceChildren(host)
      createApp({ setup: () => () => [h(Pile, { label: 'Discard', cards: [{ id: 'discard', code, name: 'Discarded card' }] }), h(CardOverlay)] })
        .use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
    }, code)
    await page.getByRole('button', { name: 'Discard', exact: true }).hover()
    const overlay = page.locator('.card-overlay')
    await expect(overlay).toBeVisible()
    const box = (await overlay.boundingBox())!
    expect(box.width > box.height).toBe(landscape)
  })
}
