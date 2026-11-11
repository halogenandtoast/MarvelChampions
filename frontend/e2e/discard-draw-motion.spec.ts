import { expect, test, type WebSocketRoute } from '@playwright/test'

const card = (id: string) => ({ id, code: '01077', name: id, owner: 'hero' })
const first = card('first'), second = card('second'), kept = card('kept')
const drawn = [card('drawn-one'), card('drawn-two')]
const snapshot = (step: number) => ({
  started: true, canUndo: true, playerId: 'hero', log: [],
  table: { owner: 1, withFriends: false, seats: 1, seated: [] },
  setup: { scenarioCode: 'rhino', modulars: [], difficulty: 'Normal' },
  game: {
    round: 2, phase: 'PlayerPhase', activePlayer: 'hero', playerOrder: ['hero'], seats: ['hero'], questionVersion: step,
    mission: null, outcome: null, accelerationTokens: 0, focusedCards: [], discardedCards: [], encounterDiscard: [], encounterDeckSize: 20,
    players: [{ id: 'hero', code: '14001b', name: 'Pietro Maximoff', faces: ['14001a', '14001b'], form: 'alterEgo', hitPoints: 9, damage: 0,
      thwart: 1, attack: 1, defense: 1, handSize: 6,
      hand: step === 0 ? [first, second, kept] : step === 1 ? [second, kept] : step === 2 ? [kept] : [kept, ...drawn],
      handCount: step === 0 ? 3 : step === 1 ? 2 : step === 2 ? 1 : 3,
      deckSize: step < 3 ? 20 : 18, discard: step === 0 ? [] : step === 1 ? [first] : [second, first], allyLimit: 3, allies: [], supports: [], upgrades: [],
    }],
    villains: [{ id: 'rhino', code: '01094', name: 'Rhino', hitPoints: 14, damage: 0, attack: 2, scheme: 1, attachments: [], upgrades: [] }],
    mainSchemes: [{ id: 'scheme', code: '01097', name: 'The Break-In!', threat: 0, threshold: 7 }], sideSchemes: [], minions: [],
    activeCost: null,
    question: { hero: { kind: 'chooseOne', prompt: step < 2 ? `$label.discardMoreFromHand count=i:${2 - step}` : null,
      choices: step < 2 ? (step === 0 ? [first, second, kept] : [second, kept]).map((c, index) => ({ index, kind: 'discard', code: c.code, target: c.id })) : [],
    } },
  },
})

test('individual discards fly to the pile before replacement cards are drawn', async ({ page }) => {
  await page.addInitScript(() => {
    localStorage.setItem('marvel-session', JSON.stringify({ token: 'test', user: { id: 1, username: 'Tester' } }))
    const start = document.startViewTransition.bind(document)
    const moves: { before: string[]; after: string[]; failed: boolean }[] = []
    ;(window as any).moves = moves
    const ghosts = () => [...document.querySelectorAll<HTMLElement>('body > img, body > div')]
      .map(el => el.style.viewTransitionName).filter(name => name.startsWith('c'))
    document.startViewTransition = ((apply: () => Promise<void>) => {
      const move = { before: ghosts(), after: [] as string[], failed: false }
      moves.push(move)
      const transition = start(async () => { await apply(); move.after = ghosts() })
      transition.ready.catch(() => { move.failed = true })
      return transition
    }) as typeof start
  })
  await page.setViewportSize({ width: 1500, height: 1000 })
  let current = snapshot(0)
  let socket: WebSocketRoute
  await page.routeWebSocket('**/api/games/discard-draw?*', ws => { socket = ws })
  await page.route('**/api/games/discard-draw', async route => {
    if (route.request().method() === 'POST') {
      if (current.game.questionVersion === 0) {
        current = snapshot(1)
        socket.send(JSON.stringify({ tag: 'Update', contents: current }))
      } else {
        current = snapshot(2)
        socket.send(JSON.stringify({ tag: 'Update', contents: current }))
        current = snapshot(3)
        socket.send(JSON.stringify({ tag: 'Update', contents: current }))
      }
    }
    await route.fulfill({ json: current })
  })
  await page.route('**/api/scenarios', route => route.fulfill({ json: { scenarios: [], modulars: [] } }))
  await page.route('**/api/whoami', route => route.fulfill({ json: { id: 1, username: 'Tester' } }))
  await page.goto('/#/games/discard-draw')
  await expect(page.locator('.in-hand')).toHaveCount(3)
  await expect(page.locator('.paying')).toHaveCount(0)
  await page.locator('.hand [data-card-id="first"] button').click()
  await expect(page.locator('.in-hand')).toHaveCount(2)
  await expect.poll(() => page.evaluate(() => (window as any).moves.some((m: any) => m.after.includes('cfirst')))).toBe(true)
  await page.locator('.hand [data-card-id="second"] button').click()
  await expect(page.locator('.hand [data-card-id="drawn-two"]')).toBeVisible()
  await expect.poll(() => page.evaluate(() => (window as any).moves.some((m: any) => m.before.includes('cdrawn-two')))).toBe(true)
  const moves = await page.evaluate(() => (window as any).moves)
  expect(moves.every((m: any) => !m.failed)).toBe(true)
  const lastDiscard = moves.findIndex((m: any) => m.after.includes('csecond'))
  const draw = moves.findIndex((m: any) => m.before.includes('cdrawn-one') && m.before.includes('cdrawn-two'))
  expect(lastDiscard).toBeGreaterThanOrEqual(0)
  expect(draw).toBeGreaterThan(lastDiscard)
  await expect(page.locator('.lane.discarded')).toHaveCount(0)
})
