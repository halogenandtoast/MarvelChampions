terraform {
  required_version = ">= 1.5.0"

  required_providers {
    digitalocean = {
      source  = "digitalocean/digitalocean"
      version = "~> 2.40"
    }
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.31"
    }
    random = {
      source  = "hashicorp/random"
      version = "~> 3.6"
    }
  }
}

provider "digitalocean" {
  token = var.do_token
}

provider "kubernetes" {
  host                   = digitalocean_kubernetes_cluster.app.endpoint
  token                  = digitalocean_kubernetes_cluster.app.kube_config[0].token
  cluster_ca_certificate = base64decode(digitalocean_kubernetes_cluster.app.kube_config[0].cluster_ca_certificate)
}
