<template>
  <div class="minion">
    <Card :card="card" :game="game" :identityId="identityId" @choose="$emit('choose', $event)" :class="{ active: activeAbility !== -1 }" @click="$emit('choose', activeAbility)" />
    <div v-if="minion.contents.minionDamage > 0" class="damage">{{minion.contents.minionDamage}}</div>
    <Upgrade
      v-for="upgrade in upgrades"
      :key="upgrade.contents.upgradeId"
      :upgrade="upgrade"
      :game="game"
      :identityId="identityId"
      class="attached"
      @choose="$emit('choose', $event)"
    />
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue'
import * as MarvelGame from '@/marvel/types/Game'
import Card from '@/marvel/components/Card.vue'
import { Game } from '@/marvel/types/Game'
import { Minion } from '@/marvel/types/Minion'
import Upgrade from '@/marvel/components/Upgrade.vue'

export default defineComponent({
  components: { Card, Upgrade },
  props: {
    game: { type: Object as () => Game, required: true },
    identityId: { type: String, required: true },
    minion: { type: Object as () => Minion, required: true },
  },
  setup(props) {
    const card = computed(() => ({ cardId: props.minion.contents.minionId, cardDef: props.minion.contents.minionCardDef }))
    const choices = computed(() => MarvelGame.choices(props.game, props.identityId))

    const activeAbility = computed(() => {
      return choices.
        value.
        findIndex((choice) => {
          if (choice.tag !== 'TargetLabel') {
            return false
          }

          const { contents } = choice.target
            if (typeof contents === "string") {
              return contents == props.minion.contents.minionId
            }

            switch (contents.tag) {
              case 'EnemyMinionId':
                return contents.contents === props.minion.contents.minionId
              default:
                return false
            }

        })
    })

    const upgrades = computed(() => props.minion.contents.minionUpgrades.map((minionId) => props.game.upgrades[minionId]))

    return { card, activeAbility, choices, upgrades }
  }
})
</script>

<style scoped lang="scss">
.minion {
  display: inline-block;
}

.attached {
  ::v-deep(img) {
    object-fit: cover;
    object-position: 0% bottom;
    height: 90px;
  }
}
</style>
