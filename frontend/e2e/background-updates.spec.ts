import { expect, test, type Page } from '@playwright/test'

test('hidden updates bypass captures and skipped captures do not reject unhandled promises', async ({ page }) => {
  const errors: string[] = []
  page.on('pageerror', error => errors.push(error.message))
  await page.goto('/#/cards/01001a')
  const result = await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { withViewTransition, skipViewTransitions } = await load('/src/composables/useViewTransition.ts')
    const start = document.startViewTransition.bind(document)
    let captures = 0
    let updates = 0
    document.startViewTransition = ((callback: () => Promise<void>) => {
      captures++
      return start(callback)
    }) as typeof document.startViewTransition
    Object.defineProperty(document, 'visibilityState', { configurable: true, value: 'hidden' })
    await withViewTransition(() => { updates++ })
    const hiddenCaptures = captures
    delete (document as any).visibilityState
    await withViewTransition(() => { skipViewTransitions(); updates++ })
    let callbackError = ''
    try {
      await withViewTransition(() => { throw new Error('state update failed') })
    } catch (error) { callbackError = (error as Error).message }
    await new Promise(resolve => setTimeout(resolve, 50))
    document.startViewTransition = start
    return { hiddenCaptures, updates, callbackError }
  })
  expect(result).toEqual({ hiddenCaptures: 0, updates: 2, callbackError: 'state update failed' })
  expect(errors).toEqual([])
})

const mountGame = async (page: Page) => {
  await page.route('**/api/games/background-test', route => route.fulfill({ json: {
    started: true, table: null, game: { round: 1, players: [], question: {} },
  } }))
  await page.goto('/#/cards/01001a')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { useGame } = await load('/src/composables/useGame.ts')
    let socket: EventTarget
    class Socket extends EventTarget {
      constructor() { super(); socket = this }
      close() {}
    }
    Object.assign(window, { WebSocket: Socket })
    const host = document.createElement('div')
    document.body.replaceChildren(host)
    const app = createApp({ setup() {
      const state = useGame('background-test')
      Object.assign(window, { gameRound: () => state.game.value?.round })
      return () => h('output', `Round ${state.game.value?.round ?? 0}`)
    } }).use(createI18n({ legacy: false, locale: 'en', messages: { en: { log: { phase: { villain: 'Villain phase' } } } } }))
    app.mount(host)
    Object.assign(window, {
      sendPhase: () => socket.dispatchEvent(new MessageEvent('message', { data: JSON.stringify({
        tag: 'Logged', entry: { seq: 1, body: [{ tag: 'LogI18n', contents: ['log.phase.villain', {}] }] },
      }) })),
      sendRound: (round: number) => socket.dispatchEvent(new MessageEvent('message', { data: JSON.stringify({
        tag: 'Update', contents: { started: true, table: null, game: { round, players: [], question: {} } },
      }) })),
      hideGame: () => {
        Object.defineProperty(document, 'hidden', { configurable: true, value: true })
        Object.defineProperty(document, 'visibilityState', { configurable: true, value: 'hidden' })
        document.dispatchEvent(new Event('visibilitychange'))
      },
      disposeGame: () => app.unmount(),
    })
  })
  await expect(page.locator('output')).toHaveText('Round 1')
}

test('hiding the tab releases animation waits and applies queued game updates promptly', async ({ page }) => {
  await mountGame(page)
  await page.evaluate(() => { (window as any).sendPhase(); (window as any).sendRound(2) })
  // Let the phase banner enter its reading delay before changing visibility.
  await page.evaluate(() => new Promise(requestAnimationFrame))
  await page.evaluate(() => (window as any).hideGame())
  await expect(page.locator('output')).toHaveText('Round 2', { timeout: 1000 })
  await page.evaluate(() => { (window as any).sendPhase(); (window as any).sendRound(3) })
  await expect(page.locator('output')).toHaveText('Round 3', { timeout: 1000 })
})

test('leaving a game drops queued updates and ignores late socket messages', async ({ page }) => {
  await mountGame(page)
  await page.evaluate(() => {
    const w = window as any
    w.sendPhase()
    w.sendRound(2)
    w.disposeGame()
    w.sendRound(3)
  })
  await page.evaluate(() => new Promise(resolve => setTimeout(resolve, 50)))
  expect(await page.evaluate(() => (window as any).gameRound())).toBe(1)
})
