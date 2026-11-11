import { expect, test } from '@playwright/test'

test('game summaries retain modulars, current identity forms, lobby state and seat ownership', async ({ request }) => {
  const response = await request.post('/api/guest', { data: {} })
  expect(response.ok()).toBeTruthy()
  const session = await response.json()
  const headers = { Authorization: `Token ${session.token}` }
  const created: string[] = []
  const scenario = { scenarioCode: '01094', modulars: ['BombScare', 'MastersOfEvil'], difficulty: 'Normal' }
  try {
    for (const seats of [1, 2]) {
      const result = await request.post('/api/games', {
        headers, data: { name: 'Game summary verification', scenario, seats, decks: [{ starter: 'spider-man' }] },
      })
      expect(result.ok()).toBeTruthy()
      created.push((await result.json()).id)
    }
    const checkActive = async () => {
      const state = await (await request.get(`/api/games/${created[0]}`, { headers })).json()
      const games = await (await request.get('/api/games', { headers })).json()
      const summary = games.find((game: any) => game.id === created[0])
      expect(summary).toMatchObject({
        modulars: scenario.modulars, waiting: false, round: state.game.round, phase: state.game.phase,
        outcome: state.game.outcome,
        seats: state.game.players.map((player: any) => ({ name: player.name, identity: player.code, damage: player.damage, eliminated: false })),
      })
      expect(games.find((game: any) => game.id === created[1])).toMatchObject({
        modulars: scenario.modulars, waiting: true, round: 0, seats: [], outcome: null,
      })
      return state.game
    }
    await checkActive()
    for (const label of [/setupMainScheme/, /doneDiscarding/, /changeForm/]) {
      const game = await checkActive()
      const [player, question] = Object.entries(game.question)[0] as [string, any]
      const choice = question.choices.find((choice: any) => label.test(choice.label ?? ''))
      expect(choice).toBeDefined()
      const answer = await request.post(`/api/games/${created[0]}`, {
        headers, data: { player, questionVersion: game.questionVersion, choice: choice.index },
      })
      expect(answer.ok()).toBeTruthy()
    }
    await checkActive()
    const other = await (await request.post('/api/guest', { data: {} })).json()
    const otherHeaders = { Authorization: `Token ${other.token}` }
    const othersGames = await (await request.get('/api/games', { headers: otherHeaders })).json()
    expect(othersGames).toEqual([])
    const joined = await request.post(`/api/games/${created[1]}/join`, {
      headers: otherHeaders, data: { deck: { starter: 'captain-marvel' } },
    })
    expect(joined.ok()).toBeTruthy()
    const seatedGames = await (await request.get('/api/games', { headers: otherHeaders })).json()
    expect(seatedGames.map((game: any) => game.id)).toEqual([created[1]])
  } finally {
    for (const id of created) await request.delete(`/api/games/${id}`, { headers })
  }
})
