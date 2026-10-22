output "cluster_name" {
  description = "DOKS cluster name. `doctl kubernetes cluster kubeconfig save <name>` for kubectl."
  value       = digitalocean_kubernetes_cluster.app.name
}

output "load_balancer_ip" {
  description = "Public IP of the load balancer in front of marvel-frontend."
  value       = local.lb_ip
}

output "app_url" {
  description = "Public URL."
  value       = "https://${var.domain_name}"
}

output "db_cluster_id" {
  description = "DigitalOcean managed Postgres cluster ID."
  value       = digitalocean_database_cluster.app.id
}

output "db_private_uri" {
  description = "Private (VPC) Postgres URI in use by the backend. Sensitive: contains the doadmin password."
  value       = digitalocean_database_connection_pool.app.private_uri
  sensitive   = true
}

output "db_public_uri" {
  description = "Public Postgres URI, reachable from IPs in db_trusted_ips. Sensitive: contains the doadmin password."
  value       = digitalocean_database_cluster.app.uri
  sensitive   = true
}

output "jwt_secret" {
  description = "JWT signing secret stored in terraform state. Sensitive."
  value       = local.jwt_secret
  sensitive   = true
}
