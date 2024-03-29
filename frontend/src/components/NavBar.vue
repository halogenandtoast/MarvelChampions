<template>
  <div id="nav">
    <span class="main-links">
      <router-link to="/" class="home-link">Home</router-link>{{' '}}
      <router-link v-if="currentUser" to="/decks">My Decks</router-link>
    </span>

    <input type="checkbox" id="dropdown-toggle" />
    <span class="user-links">
      <template v-if="currentUser">
        <label for="dropdown-toggle">
          <img :src="gravatar" class="gravatar" />
          <span>{{currentUser.username}}</span>
          <font-awesome-icon icon="angle-down" class="user-links--dropdown-icon" />
        </label>
        <div class="user-links--dropdown">
          <a href="#" @click="logout">Logout</a>
        </div>
      </template>
      <template v-else>
        <router-link to="/sign-in">Login</router-link>{{' '}}
        <router-link to="/sign-up">Register</router-link>
      </template>
    </span>
  </div>
</template>

<script lang="ts">
import { defineComponent, computed } from 'vue';
import { useStore } from 'vuex';
import { useRouter } from 'vue-router';
import { User } from '@/types';
import md5 from 'md5';
export default defineComponent({
  setup() {
    const router = useRouter()
    const store = useStore()
    const currentUser = computed<User | null>(() => store.getters.currentUser)
    const gravatar = computed(() => {
      const user = currentUser.value
      if (user) {
        const hash = md5(user.email.trim().toLowerCase());
        return `https://www.gravatar.com/avatar/${hash}?d=retro&s=22`;
      }
      return null
    })
    async function logout() {
      await store.dispatch('logout')
      router.push({ path: '/' })
    }
    return { currentUser, logout, gravatar }
  }
})
</script>

<style lang="scss" scoped>
#nav {
  height: 40px;
  box-sizing: border-box;
  display: flex;
  align-items: center;
  a {
    font-weight: bold;
    text-decoration: none;
    &:hover {
      color: #6E8640;
    }
  }
}
.main-links {
  flex-grow: 1;
  a {
    margin-left: 10px;
  }
}
.gravatar {
  height: 30px;
  margin-right: 10px;
  border-radius: 5px;
}
input[type=checkbox] {
  display: none;
}
input[type=checkbox]:checked ~ .user-links .user-links--dropdown {
  display: block;
}
input[type=checkbox]:checked ~ .user-links {
  border-radius: 5px 5px 0 0;
}
.user-links {
  display: flex;
  align-items: center;
  position: relative;
  padding-right: 10px;
  border-radius: 5px;
  label {
    user-select: none;
  }
}
.user-links--dropdown {
  display: none;
  position: absolute;
  top: 100%;
  width: 100%;
  text-align: right;
  padding: 10px;
  box-sizing: border-box;
  border-radius: 0 0 5px 5px;
}
.user-links--dropdown-icon {
  margin-left: 10px;
  cursor: pointer;
}
label {
  display: flex;
  align-items: center;
}
</style>
