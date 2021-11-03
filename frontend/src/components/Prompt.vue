<template>
  <div class="cd-popup" role="alert">
    <div class="cd-popup-container">
      <p>{{prompt}}</p>
      <ul class="cd-buttons">
         <li><a @click.prevent="yes" href="#yes">Yes</a></li>
         <li><a @click.prevent="no" href="#no">No</a></li>
      </ul>
      <a
        @click.prevent="cancelFun"
        href="#cancel"
        class="cd-popup-close img-replace">Close</a>
    </div>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue'
export default defineComponent({
  props: {
    prompt: { type: String, required: true },
    yes: { type: Function, required: true },
    no: { type: Function, required: true },
    cancel: { type: Function, required: false }
  },
  setup(props) {
    const cancelFun = computed(() => typeof props.cancel === 'function' ? props.cancel : props.no)
    return { cancelFun }
  }
})
</script>
