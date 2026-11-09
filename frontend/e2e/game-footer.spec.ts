import { expect, test } from '@playwright/test'

const hand = Array.from({ length: 7 }, (_, n) => ({ id: `hand-${n}`, code: '01077', name: `Hand card ${n + 1}` }))
const snapshot = {
  started: true, canUndo: true, playerId: 'hero', log: [],
  table: { owner: 1, withFriends: false, seats: 1, seated: [] },
  setup: { scenarioCode: 'rhino', modulars: [], difficulty: 'Normal' },
  game: {
    round: 2, phase: 'PlayerPhase', activePlayer: 'hero', playerOrder: ['hero'], seats: ['hero'], questionVersion: 3,
    mission: null, outcome: null, accelerationTokens: 0, focusedCards: [], encounterDiscard: [], encounterDeckSize: 20,
    players: [{ id: 'hero', code: '01001a', name: 'Spider-Man', faces: ['01001a', '01001b'], form: 'hero', hitPoints: 10, damage: 2,
      thwart: 1, attack: 2, defense: 3, handSize: 5, handCount: 7, hand, deckSize: 20, discard: [], allyLimit: 3, allies: [],
      supports: Array.from({ length: 9 }, (_, n) => ({ id: `support-${n}`, code: '01087', name: `Support ${n + 1}` })), upgrades: [],
    }],
    villains: [{ id: 'rhino', code: '01094', name: 'Rhino', hitPoints: 14, damage: 3, attack: 2, scheme: 1, attachments: [], upgrades: [] }],
    mainSchemes: [{ id: 'scheme', code: '01097', name: 'The Break-In!', threat: 2, threshold: 7 }], sideSchemes: [], minions: [],
    question: { hero: { kind: 'chooseOne', choices: [
      { index: 0, kind: 'play', target: 'hand-0' },
      { index: 1, kind: 'ability', target: 'hero', abilityType: 'Basic', label: '$label.attack' },
      { index: 2, kind: 'endTurn' },
    ] } },
  },
}

test('refresh fits cards before paint and clips hand overhang without a scrollbar or extra gap', async ({ page }) => {
  await page.addInitScript(() => {
    localStorage.setItem('marvel-session', JSON.stringify({ token: 'test', user: { id: 1, username: 'Tester' } }))
    const start = document.startViewTransition.bind(document)
    ;(window as any).initialTransitions = 0
    document.startViewTransition = ((...args: Parameters<typeof start>) => {
      ;(window as any).initialTransitions++
      return start(...args)
    }) as typeof start
  })
  await page.route('**/api/games/footer', route => route.fulfill({ json: snapshot }))
  await page.route('**/api/scenarios', route => route.fulfill({ json: { scenarios: [], modulars: [] } }))
  await page.route('**/api/whoami', route => route.fulfill({ json: { id: 1, username: 'Tester' } }))
  await page.routeWebSocket('**/api/games/footer?*', () => {})
  await page.goto('/#/games/footer')
  for (const height of [1000, 768]) {
    await page.setViewportSize({ width: 1600, height })
    await page.reload()
    await expect(page.locator('.in-hand')).toHaveCount(7)
    expect(await page.evaluate(() => (window as any).initialTransitions)).toBe(0)
    const samples = await page.evaluate(async () => {
      const samples = []
      for (let frame = 0; frame < 30; frame++) {
        await new Promise(requestAnimationFrame)
        const area = document.querySelector('.player-area')! as HTMLElement
        const tableau = document.querySelector('.player-tableau')!.getBoundingClientRect()
        const style = getComputedStyle(area)
        const hand = document.querySelector('.hand')!.getBoundingClientRect()
        const cards = [...document.querySelectorAll('.in-hand .card')].map(card => card.getBoundingClientRect())
        samples.push({
          overflow: tableau.width - (area.clientWidth - parseFloat(style.paddingLeft) - parseFloat(style.paddingRight)),
          overhang: Math.max(...cards.map(card => card.bottom)) - hand.bottom,
        })
      }
      return samples
    })
    expect(Math.max(...samples.map(sample => sample.overflow))).toBeLessThanOrEqual(1)
    expect(Math.min(...samples.map(sample => sample.overhang))).toBeGreaterThan(0)
    const bounds = await page.evaluate(() => {
      const board = document.querySelector('.board')! as HTMLElement
      const zoneElement = document.querySelector('.player-half')!
      const zone = zoneElement.getBoundingClientRect()
      const footer = document.querySelector('#app > footer')!.getBoundingClientRect()
      const bottom = Math.max(...[...document.querySelectorAll('.in-hand .card')].map(card => card.getBoundingClientRect().bottom))
      board.scrollTop = board.scrollHeight
      return { scroll: board.scrollHeight - board.clientHeight, overhang: bottom - zone.bottom, clipping: getComputedStyle(zoneElement).overflowY, footerGap: footer.top - zone.bottom }
    })
    expect(bounds.scroll).toBeLessThanOrEqual(1)
    expect(bounds.overhang).toBeGreaterThan(0)
    expect(bounds.clipping).toBe('clip')
    expect(bounds.footerGap).toBeLessThanOrEqual(3)

  }
})
