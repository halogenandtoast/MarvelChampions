<script setup lang="ts">
// A three-sided identity card, Ant-Man's: a folded sheet whose front cover is the
// alter-ego (B), whose back cover is one hero form (A), and whose inside spread is the
// giant hero form (C), printed sideways across both pages with its top on the left page.
// Like the card in hand, it never jumps from one side to the other: to C the cover facing
// you swings open about the spine, then the spread stands up; from C it lies down, then
// the cover of the side wanted swings shut over the other; from A to B or back it turns
// over. So a cover may end up folded either way, which is kept as its angle.
// It fills its parent, which gives it a card's shape.
import { computed, ref, watch } from 'vue'
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
// what moves first: the turn, the covers folding, the spread standing up
const delays = ref({ turn: 0, fold: 0, stand: 0 })

// one change at a time: started mid-way through another, a change turns what is still
// moving (the back of the spread shows); a pose chosen meanwhile is gone to after
let shown: Pose = pose.value
let busy = false
const advance = () => {
  const to = pose.value
  if (to === shown) return
  shown = to
  busy = true
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
    delays.value = { turn: 0, fold: 0, stand: step }
    book.value = b
    return 2 * step
  } else if (!shut(b)) {
    // the cover of the side wanted swings shut over the other, which stays put: whichever
    // way the book is turned, the leaf shut on top shows B and the page shut on top A
    if (to === 'front') b.leaf = b.page
    else b.page = b.leaf
    delays.value = { turn: 0, fold: step, stand: 0 }
    book.value = b
    return 2 * step
  }
  b.turn = 180 - b.turn
  delays.value = { turn: 0, fold: 0, stand: 0 }
  book.value = b
  return step
}

const style = computed(() => ({
  '--step': `${step}s`,
  '--d-turn': `${delays.value.turn}s`,
  '--d-fold': `${delays.value.fold}s`,
  '--d-stand': `${delays.value.stand}s`,
}))
const turnStyle = computed(() => ({ transform: `rotateY(${book.value.turn}deg)` }))
// lifted off the page after it is turned, so the lift turns with it: shut on the far side
// of the spine it lies under the page, as the page was swung over it
const leafStyle = computed(() => ({ transform: `rotateY(${book.value.leaf}deg) translateZ(1px)` }))
const pageStyle = computed(() => ({ transform: `rotateY(${book.value.page}deg)` }))
// open, the spread is centred on the spine and stood up (the other way round seen from
// the back); shut on the far side of the spine, it slides back into the card's box
const spreadStyle = computed(() => {
  const b = book.value
  if (!shut(b)) return { translate: '50% 0', scale: '0.7', rotate: b.turn === 0 ? '90deg' : '-90deg' }
  return b.leaf === 0 ? {} : { translate: '100% 0' }
})
</script>

<template>
  <div class="folding" :class="{ 'is-open': !shut(book) }" :style="style">
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
.folding { position: absolute; inset: 0; container-type: size; perspective: 1600px; border-radius: inherit; }
.turn, .spread, .page, .leaf { position: absolute; inset: 0; transform-style: preserve-3d; border-radius: inherit; }
.turn { transition: transform var(--step) ease-in-out var(--d-turn); }

/* the spine is the leaf's left edge; standing up turns about the spread's middle */
.spread {
  transform-origin: 0 50%;
  transition:
    translate var(--step) ease-in-out var(--d-fold),
    scale var(--step) ease-in-out var(--d-fold),
    rotate var(--step) ease-in-out var(--d-stand);
}
/* ponytail: 0.7 fits the upright giant (H wide, 2W tall) back into a card's box */

.leaf { transform-origin: 0 50%; transition: transform var(--step) ease-in-out var(--d-fold); }
.page { transform-origin: 0 50%; transition: transform var(--step) ease-in-out var(--d-fold); }

.face { position: absolute; inset: 0; backface-visibility: hidden; border-radius: inherit; overflow: hidden; box-shadow: 0 2px 6px rgb(0 0 0 / 0.35); }
.face.reverse { transform: rotateY(180deg); }
/* the giant is drawn only while the book is open: it appears, or goes, as the cover
   swinging over it is edge-on, half way through the fold. Shut, a cover would lie a
   pixel above it, and the browser does not always draw the nearer of the two */
.inside { visibility: hidden; transition: visibility 0s linear calc(var(--d-fold) + var(--step) / 2); }
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
