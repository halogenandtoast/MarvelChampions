<script setup lang="ts">
// A three-sided identity card, Ant-Man's: a folded sheet whose front cover is the
// alter-ego (B), whose back cover is one hero form (A), and whose inside spread is the
// giant hero form (C), printed sideways across both pages with its top on the left page.
// Like the card in hand, it never jumps from one side to the other: to C the cover facing
// you swings open about the spine, then the spread stands up; from C it lies down, then
// the cover of the side wanted swings shut over the other; from A to B or back it turns
// over. So a cover may end up folded either way, which is kept as its angle.
// It fills its parent, which gives it a card's shape.
import { computed, nextTick, ref, watch } from 'vue'
import CardFace from './CardFace.vue'

const { faces, code } = defineProps<{
  /** the sides' codes, A then B then C */
  faces: string[]
  /** the side showing */
  code: string
}>()

type Pose = 'front' | 'back' | 'open'
const pose = computed<Pose>(() => (code === faces[2] ? 'open' : code === faces[0] ? 'back' : 'front'))

// the book turned over (0 or 180), and each cover's angle about the spine: equal when
// shut, half a turn apart when open
interface Book { turn: number; leaf: number; page: number }
const shut = (b: Book) => b.leaf === b.page
const initial = (p: Pose): Book => ({ turn: p === 'back' ? 180 : 0, leaf: p === 'open' ? -180 : 0, page: 0 })
const book = ref<Book>(initial(pose.value))

const step = 0.4
// a cover swings slower than the book turns or stands, as a hand opens it
const fold = 0.6
// how far through the swing a cover is clear of the page under it: before, the two lie too
// close for the browser to always draw the nearer, so the giant under it is not drawn
const lifted = 0.12
// what moves first: the turn, the covers folding, the spread standing up
const delays = ref({ turn: 0, fold: 0, stand: 0, inside: 0 })

// one change at a time: started mid-way through another, a change turns what is still
// moving (the back of the spread shows); a pose chosen meanwhile is gone to after
let shown: Pose = pose.value
let busy = false

// a cover over on the far side of the spine is half a turn either way, the same to look
// at; but it must swing back towards you, so its angle has to be the one on your side:
// -180 seen from the front, 180 from the back. Turning the book over leaves it on the
// other, so it is set right, unseen, before it moves again
const snapping = ref(false)
const facingYou = (b: Book): Book => {
  const far = b.turn === 0 ? -180 : 180
  const side = (a: number) => (a === 0 ? 0 : far)
  return { ...b, leaf: side(b.leaf), page: side(b.page) }
}
const frame = () => new Promise((resolve) => requestAnimationFrame(() => requestAnimationFrame(resolve)))

const advance = async () => {
  const to = pose.value
  if (to === shown) return
  shown = to
  busy = true
  const set = facingYou(book.value)
  if (set.leaf !== book.value.leaf || set.page !== book.value.page) {
    snapping.value = true
    book.value = set
    await nextTick()
    await frame()
    snapping.value = false
  }
  setTimeout(() => {
    busy = false
    advance()
  }, change(to) * 1000 + 50)
}
watch(pose, () => {
  if (!busy) advance()
})

// the book changed to the pose, returning how long it takes
const change = (to: Pose): number => {
  const b = { ...book.value }
  if (to === 'open') {
    // the cover facing you swings open
    if (b.turn === 0) {
      if (b.leaf === 0) b.leaf = -180
      else b.page = 0
    } else if (b.page === 0) b.page = 180
    else b.leaf = 0
    // the giant shows as soon as the cover has lifted off it
    delays.value = { turn: 0, fold: 0, stand: fold, inside: lifted * fold }
    book.value = b
    return fold + step
  } else if (!shut(b)) {
    // the cover of the side wanted swings shut over the other, which stays put: whichever
    // way the book is turned, the leaf shut on top shows B and the page shut on top A
    if (to === 'front') b.leaf = b.page
    else b.page = b.leaf
    // and goes just before the cover lands on it
    delays.value = { turn: 0, fold: step, stand: 0, inside: step + (1 - lifted) * fold }
    book.value = b
    return fold + step
  }
  b.turn = 180 - b.turn
  delays.value = { turn: 0, fold: 0, stand: 0, inside: 0 }
  book.value = b
  return step
}

const style = computed(() => ({
  '--step': `${step}s`,
  '--fold': `${fold}s`,
  '--d-turn': `${delays.value.turn}s`,
  '--d-fold': `${delays.value.fold}s`,
  '--d-inside': `${delays.value.inside}s`,
  '--d-stand': `${delays.value.stand}s`,
}))
const turnStyle = computed(() => ({ transform: `rotateY(${book.value.turn}deg)` }))
// lifted off the page after it is turned, so the lift turns with it: shut on the far side
// of the spine it lies under the page, as the page was swung over it
const leafStyle = computed(() => ({ transform: `rotateY(${book.value.leaf}deg) translateZ(1px)` }))
const pageStyle = computed(() => ({ transform: `rotateY(${book.value.page}deg)` }))
// open, the spread is stood up (the other way round seen from the back) at its true size,
// one sideways card wide and two tall, its corner on the shut card's; the spine, its
// middle, is H/2 in and W down. Seen from the back, across is the other way. Shut on the
// far side of the spine, it slides back into the card's box
const spreadStyle = computed(() => {
  const b = book.value
  const down = 'calc(100cqw - 50cqh)'
  if (!shut(b)) return b.turn === 0 ? { translate: `50cqh ${down}`, rotate: '90deg' } : { translate: `${down} ${down}`, rotate: '-90deg' }
  return b.leaf === 0 ? {} : { translate: '100% 0' }
})
</script>

<template>
  <div class="folding" :class="{ 'is-open': !shut(book), snapping }" :style="style">
    <div class="turn" :style="turnStyle">
      <div class="spread" :style="spreadStyle">
        <div class="page" :style="pageStyle">
          <div class="face inside"><div class="half right"><CardFace :code="faces[2]" eager /></div></div>
          <div class="face reverse"><CardFace :code="faces[0]" eager /></div>
        </div>
        <div class="leaf" :style="leafStyle">
          <div class="face"><CardFace :code="faces[1]" eager /></div>
          <div class="face reverse inside"><div class="half left"><CardFace :code="faces[2]" eager /></div></div>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
/* W = 100cqw and H = 100cqh: one card; the spread is 2W by H */
/* the shut card's box, at the top left of whatever holds it: --book-w its width, or the
   holder's, which makes room for the giant itself */
.folding { --swing: cubic-bezier(0.45, 0, 0.55, 1); position: absolute; top: 0; left: 0; width: var(--book-w, 100%); aspect-ratio: 63 / 88; container-type: size; perspective: 1600px; border-radius: inherit; }
.turn, .spread, .page, .leaf { position: absolute; inset: 0; transform-style: preserve-3d; border-radius: inherit; }
.turn { transition: transform var(--step) ease-in-out var(--d-turn); }

/* the spine is the leaf's left edge; standing up turns about the spread's middle */
.spread {
  transform-origin: 0 50%;
  transition:
    translate var(--fold) var(--swing) var(--d-fold),
    scale var(--fold) var(--swing) var(--d-fold),
    rotate var(--step) ease-in-out var(--d-stand);
}

.leaf { transform-origin: 0 50%; transition: transform var(--fold) var(--swing) var(--d-fold); }
.page { transform-origin: 0 50%; transition: transform var(--fold) var(--swing) var(--d-fold); }

.face { position: absolute; inset: 0; backface-visibility: hidden; border-radius: inherit; overflow: hidden; box-shadow: 0 2px 6px rgb(0 0 0 / 0.35); }
.face.reverse { transform: rotateY(180deg); }
.snapping .leaf, .snapping .page, .snapping .spread { transition: none; }
/* the giant is drawn only while the book is open, appearing as the cover lifts off it and
   going as it lands. Shut, a cover would lie a pixel above it, and the browser does not
   always draw the nearer of the two */
.inside { visibility: hidden; transition: visibility 0s linear var(--d-inside); }
.is-open .inside { visibility: visible; }

/* the giant, turned so its top is on the left page, centred on the spine */
.half { position: absolute; inset: 0; overflow: hidden; }
.half :deep(.card-face) {
  position: absolute;
  width: 100cqh;
  max-width: none;
  height: 200cqw;
  top: calc(50cqh - 100cqw);
  rotate: -90deg;
  border-radius: 0;
}
.half.left :deep(.card-face) { left: calc(100cqw - 50cqh); }
.half.right :deep(.card-face) { left: calc(-50cqh); }

@media (prefers-reduced-motion: reduce) {
  .turn, .spread, .leaf, .page, .inside { transition: none; }
}
</style>
