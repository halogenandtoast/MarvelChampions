<script lang="ts" setup>
import { defineProps, defineEmits, computed } from 'vue'
import { Game } from '@/marvel/types/Game'
import { Villain } from '@/marvel/types/Villain'
import * as MarvelGame from '@/marvel/types/Game'
import Card from '@/marvel/components/Card.vue'
import Attachment from '@/marvel/components/Attachment.vue'
import Upgrade from '@/marvel/components/Upgrade.vue'
import AbilityButton from '@/marvel/components/AbilityButton.vue'

const props = defineProps<{
  game: Game
  identityId: string
  villain: Villain
}>()

const emit = defineEmits<{
  (e: 'choose', value: number): void
}>()

const card = computed(() => ({ cardId: props.villain.villainId, cardDef: props.villain.villainCardDef }))

const choices = computed(() => MarvelGame.choices(props.game, props.identityId))

const attachments = computed(() => props.villain.villainAttachments.map((attachmentId) => props.game.attachments[attachmentId]))

const upgrades = computed(() => props.villain.villainUpgrades.map((villainId) => props.game.upgrades[villainId]))

const abilities = computed(() => {
  return choices.value.reduce<number[]>((acc, v, i) => {
    if (v.tag === 'UseAbility' && v.contents.abilitySource.contents == props.villain.villainId) {
      return [...acc, i]
    }
    return acc
  }, [])
})
</script>

<template>
  <div class="villain">
    <Card :card="card" :game="game" :identityId="identityId" @choose="emit('choose', $event)" />
    <div class="hp">{{villain.villainHp}}</div>
    <div v-if="villain.villainStunned">Stunned</div>
    <div v-if="villain.villainConfused">Confused</div>
    <div v-if="villain.villainTough">Tough</div>
    <Attachment
      v-for="attachment in attachments"
      :key="attachment.attachmentId"
      :attachment="attachment"
      :game="game"
      :identityId="identityId"
      @choose="emit('choose', $event)"
    />
    <Upgrade
      v-for="upgrade in upgrades"
      :key="upgrade.upgradeId"
      :upgrade="upgrade"
      :game="game"
      :identityId="identityId"
      class="attached"
      @choose="emit('choose', $event)"
    />
    <AbilityButton
          v-for="ability in abilities"
          :key="ability"
          :ability="choices[ability]"
          @click="emit('choose', ability)"
          />
  </div>
</template>

<style scoped lang="scss">
.villain {
  display: inline-block;
}

.hp {
  font-family: "Exo2";
  font-weight: bold;
  font-size: 1.2em;
  background: #8E1C21;
  display: inline-block;
  padding: 0 10px;
  border: 5px solid #383839;
  color: white;
  border-radius: 10px;
}
</style>
