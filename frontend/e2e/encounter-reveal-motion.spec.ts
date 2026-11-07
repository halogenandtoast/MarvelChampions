import { expect, test } from '@playwright/test'

test('an encounter card has a visible source frame and flips while moving into the river', async ({ page }) => {
  await page.goto('/#/cards/01001a')
  const source = await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h, ref, nextTick } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: River } = await load('/src/components/River.vue')
    const { default: Pile } = await load('/src/components/Pile.vue')
    const { holdRiverSource, intoRiver } = await load('/src/composables/dealing.ts')
    const { withViewTransition } = await load('/src/composables/useViewTransition.ts')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const cards = ref([])
    const dealt = ref(true)
    const host = document.createElement('div')
    host.style.setProperty('--card-width', '124px')
    document.body.replaceChildren(host)
    createApp({ setup: () => () => [
      h(River, { cards: cards.value, elsewhere: new Set() }),
      dealt.value ? h(Pile, { label: 'Dealt encounters', pile: 'dealt-test', back: 'encounter', count: 1 }) : null,
    ] }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
    await nextTick()
    const held = holdRiverSource('encounter-test', 'dealt-test', (code: string) => [`/images/cards/${code}.avif`])
    await held.ready
    dealt.value = false
    await nextTick()
    const ghost = document.querySelector<HTMLImageElement>('[data-river-source="encounter-test"]')!
    Object.assign(window, { beginEncounterMove: () => {
      (window as any).encounterMove = (async () => {
        const move = intoRiver('encounter-test', true, (code: string) => [`/images/cards/${code}.avif`], 'dealt-test', '01186')
        await move.ready
        await withViewTransition(() => {
          move.lift()
          cards.value = [{ id: 'encounter-test', code: '01186' }]
        })
        move.done()
      })()
    } })
    return { decoded: ghost.complete && ghost.naturalWidth > 0, zIndex: getComputedStyle(ghost).zIndex }
  })
  expect(source.decoded).toBe(true)
  expect(Number(source.zIndex)).toBeGreaterThan(1)
  await expect(page.locator('[data-pile="dealt-test"]')).toHaveCount(0)
  await expect(page.locator('[data-river-source="encounter-test"]')).toBeVisible()
  await page.evaluate(() => (window as any).beginEncounterMove())
  await page.waitForFunction(() => document.getAnimations().some(animation => animation instanceof CSSAnimation && animation.animationName === 'deal-turn-up'))
  await page.evaluate(() => (window as any).encounterMove)
  const card = page.locator('.river [data-card-id="encounter-test"] img')
  await expect(card).toHaveAttribute('src', /01186/)
  expect(await card.evaluate((image: HTMLImageElement) => image.complete && image.naturalWidth > 0)).toBe(true)
})
