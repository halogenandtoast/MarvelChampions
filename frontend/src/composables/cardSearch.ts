import type { GameView, QuestionView, SearchedView } from '../server'

// Older saved searches expose their cards through focusedCards and target choices.
// Keep those searches usable alongside questions carrying explicit searched groups.
// The public view may name a choice by its action (such as addToHand).
const isCardSelection = (choice: QuestionView['choices'][number]) =>
  !choice.label && ['target', 'card', 'addToHand', 'discard', 'returnToHand', 'play'].includes(choice.kind)

export const searchedCards = (game: GameView | null | undefined, question: QuestionView): SearchedView[] => {
  if (question.searched?.length) return question.searched
  const senseDecks = (game?.players ?? []).flatMap((player) => {
    const cards = player.senseDeck ?? []
    return cards.some((card) => question.choices.some((choice) => isCardSelection(choice) && choice.target === card.id))
      ? [{ place: '$game.piles.sense', cards }]
      : []
  })
  if (senseDecks.length) return senseDecks
  const focused = game?.focusedCards ?? []
  const noMatches = question.choices.some((choice) => choice.label === '$label.noMatchingCardsFound')
  return noMatches || focused.some((card) => question.choices.some((choice) => isCardSelection(choice) && choice.target !== question.source && choice.target === card.id))
    ? [{ place: null, cards: focused }]
    : []
}

export const searchCardIds = (game: GameView) => new Set(
  Object.values(game.question ?? {}).flatMap((question) => question ? searchedCards(game, question).flatMap((group) => group.cards.map((card) => card.id)) : []),
)
