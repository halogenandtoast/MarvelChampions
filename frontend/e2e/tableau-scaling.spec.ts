import { expect, test } from '@playwright/test'

test('the tableau fits and settles after cards are added, undone, restored, and the window changes size', async ({ page }) => {
  await page.goto('/#/cards/01001a')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h, ref } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: PlayerArea } = await load('/src/components/PlayerArea.vue')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const count = ref(12)
    const host = document.createElement('div')
    host.id = 'tableau-host'
    host.style.cssText = '--card-width: 100px; width: 1000px; display: grid; grid-template-columns: minmax(0, 1fr)'
    document.body.replaceChildren(host)
    createApp({ setup: () => () => h(PlayerArea, { player: {
      id: 'player', code: '62001b', name: 'Luke Cage', faces: ['62001a', '62001b'], form: 'alterEgo',
      recover: 4, hitPoints: 10, damage: 0, handSize: 6, deckSize: 20, allyLimit: 0, dealtEncounterCards: 1,
      allies: [], supports: Array.from({ length: count.value }, (_, n) => ({ id: `support-${n}`, code: '62007', name: "Fogwell's Gym" })),
    } }) }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
    Object.assign(window, { resizeTableau: (cards: number, width: number) => { count.value = cards; host.style.width = `${width}px` } })
  })
  for (const [count, width] of [[12, 1000], [0, 1000], [12, 1000], [6, 800], [6, 1400]]) {
    await page.evaluate(([count, width]) => (window as any).resizeTableau(count, width), [count, width])
    const result = await page.evaluate(async () => {
      const samples: { scale: string; width: number; available: number }[] = []
      for (let n = 0; n < 60; n++) {
        await new Promise(requestAnimationFrame)
        const area = document.querySelector<HTMLElement>('.player-area')!
        const tableau = document.querySelector<HTMLElement>('.player-tableau')!
        const style = getComputedStyle(area)
        samples.push({ scale: tableau.style.zoom, width: tableau.getBoundingClientRect().width, available: area.clientWidth - parseFloat(style.paddingLeft) - parseFloat(style.paddingRight) })
      }
      const area = document.querySelector<HTMLElement>('.player-area')!
      const style = getComputedStyle(area)
      const piles = document.querySelector<HTMLElement>('.player-tableau > .side')!
      const supports = document.querySelector<HTMLElement>('.player-tableau > .supports')!
      return {
        scales: [...new Set(samples.slice(30).map(sample => sample.scale))], last: samples.at(-1)!,
        pilesRight: piles.getBoundingClientRect().right,
        availableRight: area.getBoundingClientRect().right - parseFloat(style.borderRightWidth) - parseFloat(style.paddingRight),
        supportGap: piles.getBoundingClientRect().left - supports.getBoundingClientRect().right,
      }
    })
    expect(result.scales).toHaveLength(1)
    expect(result.last.width).toBeLessThanOrEqual(result.last.available + 1)
    expect(result.pilesRight).toBeCloseTo(result.availableRight, 0)
    expect(result.supportGap).toBeCloseTo(14 * Number(result.last.scale), 0)
  }
})
