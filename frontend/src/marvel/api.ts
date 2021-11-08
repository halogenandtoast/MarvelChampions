import api from '@/api'
import { Deck, deckDecoder } from '@/marvel/types/Deck';
import { Game, gameDecoder } from '@/marvel/types/Game';
import { JsonDecoder } from 'ts.data.json';

export const postNewGame = (): Promise<void> => api.post('marvel/games', {})

export const fetchDecks = (): Promise<Deck[]> => api
  .get('marvel/decks')
  .then((resp) => JsonDecoder.array(deckDecoder, 'MarvelDeck[]').decodeToPromise(resp.data));

export const newDeck = (
  deckId: string,
  deckName: string,
  deckUrl: string,
): Promise<Deck> => api
  .post('marvel/decks', {
    deckId,
    deckName,
    deckUrl,
  })
  .then((resp) => deckDecoder.decodeToPromise(resp.data));

export const deleteDeck = (deckId: string): Promise<void> => api
  .delete(`marvel/decks/${deckId}`);

export const newGame = (
  deckIds: (string | null)[],
  playerCount: number,
  scenarioId: string,
  gameName: string,
  multiplayerVariant: string,
): Promise<Game> => api
  .post('marvel/games', {
    deckIds,
    playerCount,
    scenarioId,
    gameName,
    multiplayerVariant,
  })
  .then((resp) => gameDecoder.decodeToPromise(resp.data));

interface FetchData {
  identityId: string
  game: Game
}

export const fetchGame = (gameId: string): Promise<FetchData> => api
  .get(`marvel/games/${gameId}`)
  .then((resp) => {
    const { identityId, game } = resp.data;
    return gameDecoder
      .decodeToPromise(game)
      .then((gameData) => Promise.resolve({ identityId, game: gameData }));
  });

export const fetchGames = (): Promise<Game[]> => api
  .get('marvel/games')
  .then((resp) => JsonDecoder.array(gameDecoder, 'MarvelGame[]').decodeToPromise(resp.data));

export const updateGame = (gameId: string, choice: number, investigatorId: string | null): Promise<void> => api
  .put(`marvel/games/${gameId}`, {tag: 'Answer', contents: { choice, investigatorId }})

export const deleteGame = (gameId: string): Promise<void> => api
  .delete(`marvel/games/${gameId}`);

export const undoStep = (gameId: string): Promise<void> => api
  .put(`marvel/games/${gameId}/undo`)
