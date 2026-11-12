import { expect, test, type WebSocketRoute } from '@playwright/test'

test('Spider-Sense draws from the player deck after the attack banner leaves', async ({ page }) => {
  await page.addInitScript(() => {
    localStorage.setItem('marvel-session', JSON.stringify({ token: 'test', user: { id: 1, username: 'Tester' } }))
    const start = document.startViewTransition.bind(document)
    const moves: any[] = []
    Object.assign(window, { senseMoves: moves })
    document.startViewTransition = ((apply: () => Promise<void>) => {
      const source = document.querySelector<HTMLImageElement>('body > img[style*="csense-draw"]')
      const deck = document.querySelector('[data-pile="deck-hero"] .stack')
      const move = { source: source ? { src: source.src, x: source.getBoundingClientRect().x, y: source.getBoundingClientRect().y } : null,
        deck: deck ? { x: deck.getBoundingClientRect().x, y: deck.getBoundingClientRect().y } : null, failed: false }
      moves.push(move)
      const transition = start(apply)
      transition.ready.catch(() => { move.failed = true })
      return transition
    }) as typeof start
  })
  await page.setViewportSize({ width: 1500, height: 1000 })
  const kept = { id: 'kept', code: '01077', name: 'kept', owner: 'hero' }
  const drawn = { id: 'sense-draw', code: '01009', name: 'Webbed Up', owner: 'hero' }
  const snapshot = (after: boolean, final = false) => ({
    started: true, canUndo: true, log: [],
    table: { owner: 1, withFriends: false, seats: 1, seated: [] },
    setup: { scenarioCode: '01094', modulars: ['BombScare'], difficulty: 'Normal' },
    game: {
      round: 1, phase: 'VillainPhase', activePlayer: 'hero', playerOrder: ['hero'], seats: ['hero'], questionVersion: final ? 2 : 1,
      mission: null, outcome: null, accelerationTokens: 0, focusedCards: [], encounterDiscard: [], encounterDeckSize: 20,
      players: [{ id: 'hero', code: '01001a', name: 'Spider-Man', faces: ['01001a', '01001b'], form: 'hero', hitPoints: 10, damage: 0,
        thwart: 1, attack: 2, defense: 3, handSize: 5, hand: after ? [kept, drawn] : [kept], handCount: after ? 2 : 1,
        deckSize: after ? 19 : 20, discard: [], allyLimit: 3, allies: [], supports: [], upgrades: [],
      }],
      villains: [{ id: 'rhino', code: '01094', name: 'Rhino', hitPoints: 14, damage: 0, attack: 2, scheme: 1, attachments: [], upgrades: [] }],
      mainSchemes: [{ id: 'scheme', code: '01097', name: 'The Break-In!', threat: 1, threshold: 7 }], sideSchemes: [], minions: [],
      question: { hero: { kind: 'chooseOne', prompt: final ? '$label.declareDefender' : '$label.respond', choices: final
        ? [{ index: 0, kind: 'label', label: '$label.noDefender' }]
        : [{ index: 0, kind: 'label', label: '$label.noResponse' }, { index: 1, kind: 'ability', abilityType: 'Interrupt', label: 'Spider-Sense', target: 'hero' }],
      } },
    },
  })
  let current = snapshot(false)
  let socket: WebSocketRoute
  await page.routeWebSocket('**/api/games/spider-sense?*', ws => { socket = ws })
  await page.route('**/api/games/spider-sense', async route => {
    if (route.request().method() === 'POST') {
      socket.send(JSON.stringify({ tag: 'Logged', entry: {
        seq: 1, step: null, kind: 'Action', children: [], source: null, group: null, context: null, tone: null, audience: { tag: 'Everyone' },
        body: [{ tag: 'LogI18n', contents: ['log.attacks', {
          enemy: { tag: 'LogRefPart', contents: { kind: 'RefVillain', name: 'Rhino', cardCode: '01094', entityId: 'rhino' } },
          player: { tag: 'LogRefPart', contents: { kind: 'RefIdentity', name: 'Spider-Man', cardCode: '01001a', entityId: 'hero' } },
        }] }],
      } }))
      socket.send(JSON.stringify({ tag: 'Update', contents: snapshot(true) }))
      socket.send(JSON.stringify({ tag: 'BoostDealt', cardId: 'boost' }))
      current = snapshot(true, true)
      socket.send(JSON.stringify({ tag: 'Update', contents: current }))
    }
    await route.fulfill({ json: current })
  })
  await page.route('**/api/whoami', route => route.fulfill({ json: { id: 1, username: 'Tester' } }))
  await page.route('**/api/scenarios', route => route.fulfill({ json: { scenarios: [], modulars: [] } }))
  await page.goto('/#/games/spider-sense')
  await page.locator('.reaction [data-card-id="hero"] .chip').click()
  await expect(page.locator('.fight-banner')).toBeVisible()
  await expect(page.locator('.hand [data-card-id="sense-draw"]')).toBeVisible()
  await page.waitForFunction(() => document.getAnimations().some(animation =>
    animation instanceof CSSAnimation && animation.animationName === 'deal-turn-up'))
  await expect.poll(() => page.evaluate(() => (window as any).senseMoves.some((move: any) => move.source))).toBe(true)
  const move = await page.evaluate(() => (window as any).senseMoves.find((move: any) => move.source))
  expect(move.source.src).toContain('back-player')
  expect(move.source.x).toBeCloseTo(move.deck.x, 0)
  expect(move.source.y).toBeCloseTo(move.deck.y, 0)
  expect(move.failed).toBe(false)
})
