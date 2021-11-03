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
  .post('arkham/games', {
    deckIds,
    playerCount,
    scenarioId,
    gameName,
    multiplayerVariant,
  })
  .then((resp) => gameDecoder.decodeToPromise(resp.data));
