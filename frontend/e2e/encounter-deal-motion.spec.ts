import { expect, test } from '@playwright/test'

for (const initial of [0, 2]) {
  test(`dealing an encounter keeps the previous ${initial} cards until landing`, async ({ page }) => {
    await page.goto('/#/cards/01001a')
    const during = await page.evaluate(async (initial) => {
      const load = (path: string) => import(/* @vite-ignore */ path)
      const { createApp, h, ref, nextTick } = await load('/node_modules/.vite/deps/vue.js')
      const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
      const { default: Pile } = await load('/src/components/Pile.vue')
      const componentSource = await (await fetch('/src/components/Pile.vue')).text()
      const dealingPath = componentSource.match(/from ["']([^"']*\/composables\/dealing[^"']*)["']/)?.[1]
      if (!dealingPath) throw new Error('Pile dealing import not found')
      const { dealFrom } = await load(dealingPath)
      const { withViewTransition } = await load('/src/composables/useViewTransition.ts')
      const { loadLocaleMessages } = await load('/src/locales/messages.ts')
      const { messages } = await loadLocaleMessages('en')
      const oldIds = Array.from({ length: initial }, (_, n) => `old-${n}`)
      const before = { players: [{ id: 'test', dealtEncounterCards: initial, dealtCardIds: oldIds }] }
      const after = { players: [{ id: 'test', dealtEncounterCards: initial + 1, dealtCardIds: [...oldIds, 'new-card'] }] }
      const count = ref(initial)
      const host = document.createElement('div')
      host.style.cssText = '--card-width: 100px; display: flex; gap: 300px'
      document.body.replaceChildren(host)
      createApp({ setup: () => () => [
        h(Pile, { label: 'Encounter deck', pile: 'deck-encounter', back: 'encounter', count: 20 }),
        count.value ? h(Pile, { label: 'Dealt', pile: 'dealt-test', back: 'encounter', count: count.value }) : null,
      ] }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
      await nextTick()
      const move = dealFrom(before, after, () => [])
      let during: { count: string | null; visibility: string; width: number } | null = null
      await withViewTransition(async () => {
        move.lift()
        count.value++
        await nextTick()
        move.land()
        const pile = document.querySelector<HTMLElement>('[data-pile="dealt-test"]')!
        during = { count: pile.querySelector('.count')!.textContent, visibility: getComputedStyle(pile).visibility, width: pile.getBoundingClientRect().width }
      })
      move.done()
      await nextTick()
      return during
    }, initial)
    expect(during).toMatchObject({ count: String(initial), visibility: initial ? 'visible' : 'hidden' })
    expect(during!.width).toBeGreaterThan(0)
    const pile = page.locator('[data-pile="dealt-test"]')
    await expect(pile).toBeVisible()
    await expect(pile.locator('.count')).toHaveText(String(initial + 1))
    await expect(page.locator('body > img')).toHaveCount(0)
  })
}
