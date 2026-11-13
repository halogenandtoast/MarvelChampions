import { expect, test } from '@playwright/test'

test('deck shuffles follow the displayed area, including when all seats are rendered', async ({ page }) => {
  await page.route('**/api/games/area-animation-test', route => route.fulfill({ json: {
    started: true, table: null, game: { round: 1, players: [], question: {}, areas: [
      { id: 1, players: ['groot'] }, { id: 2, players: ['strange'] },
    ] },
  } }))
  await page.goto('/#/cards/01001a')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { useGame } = await load('/src/composables/useGame.ts')
    const { default: Pile } = await load('/src/components/Pile.vue')
    let socket: EventTarget
    class Socket extends EventTarget {
      constructor() { super(); socket = this }
      close() {}
    }
    Object.assign(window, { WebSocket: Socket })
    const host = document.createElement('div')
    host.style.cssText = '--card-width:100px;display:flex;gap:50px;padding:100px'
    document.body.replaceChildren(host)
    let area = 1
    const shuffled: string[] = []
    const animate = Element.prototype.animate
    Element.prototype.animate = function (...args: Parameters<typeof animate>) {
      shuffled.push((this as HTMLImageElement).src)
      return animate.apply(this, args)
    }
    createApp({ setup() {
      const state = useGame('area-animation-test', false, () => area)
      return () => [h('output', state.game.value ? 'loaded' : 'loading'),
        ...['groot', 'strange', 'encounter'].map(id => h(Pile, { label: id, pile: `deck-${id}`, back: 'player', count: 10 })),
      ]
    } }).use(createI18n({ legacy: false, locale: 'en', messages: {} })).mount(host)
    Object.assign(window, {
      showAnimationArea: (id: number) => { area = id },
      shuffleAreaDeck: (id: string) => socket.dispatchEvent(new MessageEvent('message', { data: JSON.stringify({ tag: 'Shuffled', pile: `deck-${id}` }) })),
      shuffleAnimations: () => shuffled.length,
    })
  })
  await expect(page.locator('output')).toHaveText('loaded')
  await page.evaluate(() => (window as any).shuffleAreaDeck('strange'))
  await page.waitForTimeout(100)
  expect(await page.evaluate(() => (window as any).shuffleAnimations())).toBe(0)
  await page.evaluate(() => (window as any).shuffleAreaDeck('groot'))
  await expect.poll(() => page.evaluate(() => (window as any).shuffleAnimations())).toBe(10)
  await expect.poll(() => page.locator('[data-pile="deck-groot"] .stack').evaluate(el => (el as HTMLElement).style.visibility)).toBe('')
  await page.evaluate(() => { (window as any).showAnimationArea(2); (window as any).shuffleAreaDeck('strange') })
  await expect.poll(() => page.evaluate(() => (window as any).shuffleAnimations())).toBe(20)
  await page.evaluate(() => (window as any).shuffleAreaDeck('encounter'))
  await expect.poll(() => page.evaluate(() => (window as any).shuffleAnimations())).toBe(30)
})

test('drawing in another area does not animate from a cached deck position', async ({ page }) => {
  await page.goto('/#/cards/01001a')
  const result = await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { dealFrom, notePiles } = await load('/src/composables/dealing.ts')
    document.body.innerHTML = '<div data-pile="deck-strange"><div class="stack" style="width:100px;height:140px"></div></div>'
    notePiles()
    document.body.replaceChildren()
    const before = {
      players: [{ id: 'groot', hand: [] }, { id: 'strange', hand: [] }],
      villains: [], minions: [], mainSchemes: [], sideSchemes: [],
      areas: [{ id: 1, players: ['groot'], villains: [], mainSchemes: [], sideSchemes: [], environments: [] },
        { id: 2, players: ['strange'], villains: [], mainSchemes: [], sideSchemes: [], environments: [] }],
    }
    const after = { ...before, players: [before.players[0], { id: 'strange', hand: [{ id: 'drawn', code: '09007' }] }] }
    const hidden = dealFrom(before, after, () => [], new Map(), 1)
    const hiddenGhosts = document.querySelectorAll('body > img').length
    hidden?.done()
    const visible = dealFrom(before, after, () => [], new Map(), 2)
    const visibleGhosts = document.querySelectorAll('body > img').length
    visible?.done()
    return { hiddenGhosts, visibleGhosts }
  })
  expect(result).toEqual({ hiddenGhosts: 0, visibleGhosts: 1 })
})
