<script setup lang="ts">
// A three-sided identity card, Ant-Man's: a folded sheet whose front cover is the
// alter-ego (B), whose back cover is one hero form (A), and whose inside spread is the
// giant hero form (C), printed sideways across both pages with its top on the left page.
// Changing to C opens the cover around the left spine, then turns the spread upright;
// leaving C turns it back and folds it shut, then turns the book over if A is next.
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

// each stage waits for the one before it: open = turn over and unfold, then stand up;
// close = lie down, then fold, then turn over
const step = 0.55
const delays = ref({ turn: 0, fold: 0, stand: 0 })
watch(pose, (to, from) => {
  if (to === 'open') delays.value = { turn: 0, fold: 0, stand: step }
  else if (from === 'open') delays.value = { turn: 2 * step, fold: step, stand: 0 }
  else delays.value = { turn: 0, fold: 0, stand: 0 }
})
const style = computed(() => ({
  '--step': `${step}s`,
  '--d-turn': `${delays.value.turn}s`,
  '--d-fold': `${delays.value.fold}s`,
  '--d-stand': `${delays.value.stand}s`,
}))
</script>

<template>
  <div class="folding" :class="pose" :style="style">
    <div class="turn">
      <div class="spread">
        <div class="page">
          <div class="face"><div class="half right"><CardFace :code="faces[2]" eager /></div></div>
          <div class="face reverse"><CardFace :code="faces[0]" eager /></div>
        </div>
        <div class="leaf">
          <div class="face"><CardFace :code="faces[1]" eager /></div>
          <div class="face reverse"><div class="half left"><CardFace :code="faces[2]" eager /></div></div>
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
.back .turn { transform: rotateY(180deg); }

/* the spine is the leaf's left edge; standing up turns about the spread's middle */
.spread {
  transform-origin: 0 50%;
  transition:
    translate var(--step) ease-in-out var(--d-fold),
    scale var(--step) ease-in-out var(--d-fold),
    rotate var(--step) ease-in-out var(--d-stand);
}
/* ponytail: 0.7 fits the upright giant (H wide, 2W tall) back into a card's box */
.open .spread { translate: 50% 0; scale: 0.7; rotate: 90deg; }

.leaf { transform: translateZ(1px); transform-origin: 0 50%; transition: transform var(--step) ease-in-out var(--d-fold); }
.open .leaf { transform: translateZ(1px) rotateY(-180deg); }

.face { position: absolute; inset: 0; backface-visibility: hidden; border-radius: inherit; overflow: hidden; box-shadow: 0 2px 6px rgb(0 0 0 / 0.35); }
.face.reverse { transform: rotateY(180deg); }

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
  .turn, .spread, .leaf { transition: none; }
}
</style>
