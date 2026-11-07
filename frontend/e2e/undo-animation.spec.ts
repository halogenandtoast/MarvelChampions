import { expect, test } from '@playwright/test'

test('undo interrupts the active banner and drops queued animation states', async ({ page }) => {
  let undone = false
  const state = (version: number) => ({
    canUndo: true, started: true, table: null, log: [],
    game: { questionVersion: version, players: [], villains: [], minions: [], mainSchemes: [], sideSchemes: [], environments: [], boostCards: [], focusedCards: [], question: {} },
  })
  await page.route('**/api/games/undo-animation-test**', async route => {
    if (route.request().method() === 'POST') {
      undone = true
      await route.fulfill({ status: 200, contentType: 'application/json', body: '{}' })
    } else await route.fulfill({ json: state(undone ? 0 : 1) })
  })
  await page.goto('/#/cards/01001a')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { useGame } = await load('/src/composables/useGame.ts')
    const { default: River } = await load('/src/components/River.vue')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    class Socket extends EventTarget {
      constructor() {
        super()
        Object.assign(window, { animationSocket: this })
        setTimeout(() => this.dispatchEvent(new Event('open')), 0)
      }
      close() {}
    }
    Object.assign(window, { WebSocket: Socket })
    const host = document.createElement('div')
    document.body.replaceChildren(host)
    createApp({ setup() {
      const game = useGame('undo-animation-test')
      Object.assign(window, { undoTestGame: () => game.undo() })
      return () => [
        h('button', { onClick: () => game.undo() }, 'Undo'),
        h('output', { id: 'version' }, String(game.game.value?.questionVersion ?? 'loading')),
        h(River, { cards: game.river.value, elsewhere: new Set(), activationAlert: game.activationAlert.value, activationBanner: game.activationBanner.value, activationKind: game.activationKind.value }),
      ]
    } }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
  })
  await expect(page.locator('#version')).toHaveText('1')
  await page.evaluate(next => {
    const socket = (window as any).animationSocket
    const send = (message: unknown) => socket.dispatchEvent(new MessageEvent('message', { data: JSON.stringify(message) }))
    for (const [key, targetKey] of [['log.takesDamage', 'target'], ['log.threatPlaced', 'scheme']]) {
      send({ tag: 'Logged', entry: { seq: 1, body: [{ tag: 'LogI18n', contents: [key, {
        [targetKey]: { tag: 'LogRefPart', contents: { name: 'Target' } }, count: { tag: 'LogNumber', contents: 2 },
      }] }] } })
    }
    send({ tag: 'Update', contents: next })
  }, state(2))
  await expect(page.locator('#version')).toHaveText('2', { timeout: 700 })
  await expect(page.locator('.river').getByRole('status')).toHaveCount(0)
  await page.evaluate(old => {
    const socket = (window as any).animationSocket
    const send = (message: unknown) => socket.dispatchEvent(new MessageEvent('message', { data: JSON.stringify(message) }))
    send({ tag: 'Logged', entry: { seq: 1, body: [{ tag: 'LogI18n', contents: ['log.attacks', {
      enemy: { tag: 'LogRefPart', contents: { kind: 'RefVillain', name: 'Venom Goblin' } },
      player: { tag: 'LogRefPart', contents: { name: 'Daredevil' } },
    }] }] } })
    send({ tag: 'Update', contents: old })
  }, state(3))
  const alert = page.locator('.river').getByRole('status')
  await expect(alert).toContainText('Venom Goblin attacks')
  await page.getByRole('button', { name: 'Undo', exact: true }).click()
  await expect(alert).toHaveCount(0, { timeout: 700 })
  await expect(page.locator('#version')).toHaveText('0', { timeout: 700 })
  // The old animation's hold time must not allow its queued state to reappear.
  await page.waitForTimeout(1300)
  await expect(page.locator('#version')).toHaveText('0')
  expect(undone).toBe(true)
  for (const phase of ['player', 'villain']) {
    await page.evaluate(phase => {
      (window as any).animationSocket.dispatchEvent(new MessageEvent('message', { data: JSON.stringify({
        tag: 'Logged', entry: { seq: 2, body: [{ tag: 'LogI18n', contents: [`log.phase.${phase}`, {}] }] },
      }) }))
    }, phase)
    await expect(alert).toContainText(phase === 'player' ? 'Player phase' : 'Villain phase')
    await expect(alert).toHaveClass(/fight-banner/)
    if (phase === 'player') await expect(alert).toHaveClass(/scheme-banner/)
    else await expect(alert).not.toHaveClass(/scheme-banner/)
    await page.evaluate(() => (window as any).undoTestGame())
    await expect(alert).toHaveCount(0)
  }
})
