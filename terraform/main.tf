locals {
  tags = [var.name]
}

# --- Cluster -----------------------------------------------------------------

data "digitalocean_kubernetes_versions" "current" {}

resource "digitalocean_kubernetes_cluster" "app" {
  name    = var.name
  region  = var.region
  version = data.digitalocean_kubernetes_versions.current.latest_version
  tags    = local.tags

  node_pool {
    name       = "${var.name}-pool"
    size       = var.node_size
    node_count = var.node_count
  }

  # The latest version moves on its own; upgrade on purpose, not on the next
  # apply.
  lifecycle {
    ignore_changes = [version]
  }
}

# --- Database ----------------------------------------------------------------

resource "random_password" "jwt_secret" {
  length      = 48
  special     = false
  min_lower   = 4
  min_upper   = 4
  min_numeric = 4
}

locals {
  jwt_secret = var.jwt_secret_override == "" ? random_password.jwt_secret.result : var.jwt_secret_override
}

resource "digitalocean_database_cluster" "app" {
  name       = "${var.name}-db"
  engine     = "pg"
  version    = var.db_engine_version
  size       = var.db_size
  region     = var.region
  node_count = var.db_node_count
  tags       = local.tags
}

# Connection pool: smaller open-connection count to the actual DB. The app
# uses this pool's private_uri (resolved over the VPC, no public traffic).
resource "digitalocean_database_connection_pool" "app" {
  cluster_id = digitalocean_database_cluster.app.id
  name       = "${var.name}-pool"
  mode       = "transaction"
  size       = 22
  db_name    = digitalocean_database_cluster.app.database
  user       = digitalocean_database_cluster.app.user
}

# Only the cluster's nodes may reach the database.
resource "digitalocean_database_firewall" "app" {
  cluster_id = digitalocean_database_cluster.app.id

  rule {
    type  = "k8s"
    value = digitalocean_kubernetes_cluster.app.id
  }

  dynamic "rule" {
    for_each = var.db_trusted_ips
    content {
      type  = "ip_addr"
      value = rule.value
    }
  }
}

# --- In-cluster --------------------------------------------------------------

# Read by marvel-backend (k8s/app.yaml) through envFrom.
resource "kubernetes_secret" "app" {
  metadata {
    name = "marvel"
  }

  data = merge(
    {
      DATABASE_URL = digitalocean_database_connection_pool.app.private_uri
      JWT_SECRET   = local.jwt_secret
    },
    var.app_secrets,
  )
}

# The public entry point. DigitalOcean's cloud controller turns this into a
# load balancer that terminates TLS with the certificate below and forwards
# plain HTTP to Caddy in marvel-frontend. It lives here rather than in
# k8s/app.yaml so its IP can feed the DNS records.
resource "kubernetes_service" "public" {
  metadata {
    name = "marvel-public"
    annotations = {
      "service.beta.kubernetes.io/do-loadbalancer-name"                             = var.name
      "service.beta.kubernetes.io/do-loadbalancer-type"                             = "REGIONAL" # the default network LB is TCP-only and ignores the TLS settings
      "service.beta.kubernetes.io/do-loadbalancer-protocol"                         = "http"
      "service.beta.kubernetes.io/do-loadbalancer-tls-ports"                        = "443"
      "service.beta.kubernetes.io/do-loadbalancer-certificate-name"                 = digitalocean_certificate.app.name
      "service.beta.kubernetes.io/do-loadbalancer-redirect-http-to-https"           = "true"
      "service.beta.kubernetes.io/do-loadbalancer-http-idle-timeout-seconds"        = "600" # game websockets
      "service.beta.kubernetes.io/do-loadbalancer-disable-lets-encrypt-dns-records" = "true"
    }
  }

  spec {
    type     = "LoadBalancer"
    selector = { app = "marvel-frontend" }

    port {
      name        = "http"
      port        = 80
      target_port = 80
    }

    port {
      name        = "https"
      port        = 443
      target_port = 80
    }
  }
  # DigitalOcean's controller writes these back onto the service. Dropping the
  # load balancer id would orphan the load balancer.
  lifecycle {
    ignore_changes = [
      metadata[0].annotations["kubernetes.digitalocean.com/load-balancer-id"],
    ]
  }
}

# --- DNS + TLS ---------------------------------------------------------------

resource "digitalocean_domain" "app" {
  name = var.domain_name
}

resource "digitalocean_certificate" "app" {
  name    = "${var.name}-cert"
  type    = "lets_encrypt"
  domains = [var.domain_name, "www.${var.domain_name}"]

  lifecycle {
    create_before_destroy = true
  }

  depends_on = [digitalocean_domain.app]
}

locals {
  lb_ip = kubernetes_service.public.status[0].load_balancer[0].ingress[0].ip
}

resource "digitalocean_record" "apex_a" {
  domain = digitalocean_domain.app.id
  type   = "A"
  name   = "@"
  value  = local.lb_ip
  ttl    = 300
}

resource "digitalocean_record" "www_cname" {
  domain = digitalocean_domain.app.id
  type   = "CNAME"
  name   = "www"
  value  = "${var.domain_name}."
  ttl    = 300
}

resource "digitalocean_project" "app" {
  name        = var.name
  description = "Marvel Champions: The Card Game online"
  purpose     = "Web Application"
  environment = "Production"
  resources = [
    digitalocean_kubernetes_cluster.app.urn,
    digitalocean_database_cluster.app.urn,
    digitalocean_domain.app.urn,
  ]
}
