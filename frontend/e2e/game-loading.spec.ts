import { expect, test } from '@playwright/test'

test('loads the board while the WebSocket handshake is pending', async ({ page }) => {
  await page.route('**/api/games/loading-test', route => route.fulfill({ json: {
    started: true, canUndo: false, table: null,
    game: { round: 3, players: [], question: {} },
  } }))
  await page.goto('/#/cards/01001a')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { useGame } = await load('/src/composables/useGame.ts')
    class Socket extends EventTarget { close() {} }
    Object.assign(window, { WebSocket: Socket })
    const host = document.createElement('div')
    document.body.replaceChildren(host)
    createApp({ setup() {
      const game = useGame('loading-test')
      return () => h('output', game.game.value ? `Round ${game.game.value.round}` : 'loading')
    } }).use(createI18n({ legacy: false, locale: 'en', messages: { en: {} } })).mount(host)
  })
  await expect(page.locator('output')).toHaveText('Round 3')
})
