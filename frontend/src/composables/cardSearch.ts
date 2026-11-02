import type { GameView, QuestionView, SearchedView } from '../server'

// Older saved searches expose their cards through focusedCards and target choices.
// Keep those searches usable alongside questions carrying explicit searched groups.
export const searchedCards = (game: GameView | null | undefined, question: QuestionView): SearchedView[] => {
  if (question.searched?.length) return question.searched
  const focused = game?.focusedCards ?? []
  const noMatches = question.choices.some((choice) => choice.label === '$label.noMatchingCardsFound')
  return noMatches || focused.some((card) => question.choices.some((choice) => choice.kind === 'target' && !choice.label && choice.target !== question.source && choice.target === card.id))
    ? [{ place: null, cards: focused }]
    : []
}

export const searchCardIds = (game: GameView) => new Set(
  Object.values(game.question ?? {}).flatMap((question) => question ? searchedCards(game, question).flatMap((group) => group.cards.map((card) => card.id)) : []),
)
