variable "do_token" {
  description = "DigitalOcean API token with write access. Prefer passing via DIGITALOCEAN_TOKEN env var or a .tfvars file kept out of git."
  type        = string
  sensitive   = true
  default     = null
}

variable "name" {
  description = "Base name applied to the cluster, database, project, and tags."
  type        = string
  default     = "marvel-champions"
}

variable "region" {
  description = "DigitalOcean region slug. See https://slugs.do-api.dev/."
  type        = string
  default     = "nyc3"
}

variable "node_size" {
  description = "Droplet size slug for the cluster's node pool."
  type        = string
  default     = "s-2vcpu-2gb"
}

variable "node_count" {
  description = "Nodes in the pool."
  type        = number
  default     = 1
}

variable "domain_name" {
  description = "Public domain. Terraform creates it in DigitalOcean DNS, points it at the load balancer, and issues a Let's Encrypt certificate for it."
  type        = string
  default     = "marvelchampions.app"
}

# --- Database ----------------------------------------------------------------

variable "db_size" {
  description = "DigitalOcean managed Postgres node slug. db-s-1vcpu-1gb is the cheapest tier suitable for prod-like workloads."
  type        = string
  default     = "db-s-1vcpu-1gb"
}

variable "db_node_count" {
  description = "Number of nodes in the DB cluster. 1 = single-node (no HA). Raise to 2+ for failover."
  type        = number
  default     = 1
}

variable "db_engine_version" {
  description = "PostgreSQL major version supported by DigitalOcean."
  type        = string
  default     = "16"
}

variable "db_trusted_ips" {
  description = "Extra IPs allowed through the database firewall, such as your own for psql."
  type        = list(string)
  default     = []
}

variable "jwt_secret_override" {
  description = "Override the auto-generated JWT signing secret. Leave empty to let terraform generate one and keep it in state."
  type        = string
  default     = ""
  sensitive   = true
}

variable "app_secrets" {
  description = "Free-form app secrets to mount as env, such as AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY for bug uploads. Keys become env var names."
  type        = map(string)
  sensitive   = true
  default     = {}
}
