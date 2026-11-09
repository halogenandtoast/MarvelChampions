import { expect, test } from '@playwright/test'

test('recovers the payment question when the final socket update is missed', async ({ page }) => {
  let paid = false
  const state = () => ({
    canUndo: true, started: true, table: null,
    game: {
      questionVersion: paid ? 2 : 1, players: [],
      question: { player: { prompt: paid ? '$label.pay' : '$label.yourTurn', choices: [] } },
    },
  })
  await page.route('**/api/games/payment-resync-test**', async route => {
    if (route.request().method() === 'POST') {
      paid = true
      await route.fulfill({ status: 200, body: '{}' })
    } else await route.fulfill({ json: state() })
  })
  await page.goto('/#/cards/01001a')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { useGame } = await load('/src/composables/useGame.ts')
    // Simulate a connection which opens but loses the move's broadcast.
    class Socket extends EventTarget {
      constructor() {
        super()
        setTimeout(() => this.dispatchEvent(new Event('open')), 0)
      }
      close() {}
    }
    Object.assign(window, { WebSocket: Socket })
    const host = document.createElement('div')
    document.body.replaceChildren(host)
    createApp({ setup() {
      const game = useGame('payment-resync-test')
      return () => [
        h('button', { onClick: () => game.answer(1, 0, 'player') }, 'Pay resource'),
        h('output', game.game.value?.question.player?.prompt ?? 'loading'),
      ]
    } }).use(createI18n({ legacy: false, locale: 'en', messages: { en: {} } })).mount(host)
  })
  await expect(page.locator('output')).toHaveText('$label.yourTurn')
  await page.getByRole('button', { name: 'Pay resource' }).click()
  await expect(page.locator('output')).toHaveText('$label.pay')
})
