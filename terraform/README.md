# Terraform — DigitalOcean Kubernetes deploy

## What it creates

- A DOKS cluster with one `s-2vcpu-2gb` node in `nyc3`.
- A managed Postgres 16 cluster (`db-s-1vcpu-1gb`, single node) plus a
  transaction-mode connection pool, firewalled to the Kubernetes cluster.
- The `marvel` secret in the cluster: `DATABASE_URL` (the pool's private URI)
  and a generated `JWT_SECRET`.
- The `marvel-public` LoadBalancer service. DigitalOcean makes it a load
  balancer that terminates TLS with a Let's Encrypt certificate and sends
  plain HTTP to Caddy in `marvel-frontend`.
- The domain in DigitalOcean DNS, an A record for the load balancer, and a
  `www` CNAME.

The app's deployments are in `../k8s/app.yaml`, applied by
`../scripts/deploy.sh`. The backend creates its tables on start
(`runMigration`), so there is no separate migration step.

## Usage

```sh
cd terraform
export DIGITALOCEAN_TOKEN=dop_v1_...
cp terraform.tfvars.example terraform.tfvars
$EDITOR terraform.tfvars

terraform init
terraform apply
```

Point the registrar's nameservers at `ns{1,2,3}.digitalocean.com`.

If `marvelchampions.app` was already added in the DigitalOcean console, import
it before the first apply:

```sh
terraform import digitalocean_domain.app marvelchampions.app
```

## Deploying

```sh
docker login        # one-time, against Docker Hub
doctl auth init     # one-time
scripts/deploy.sh   # build amd64 -> push -> kubectl apply
```

The first `terraform apply` leaves the load balancer without pods behind it
until the first deploy.

## Things this does NOT do

- No remote state. `terraform.tfstate` holds the JWT secret and DB password;
  treat it as a secret.
- AWS assets hosting is managed separately with the AWS CLI, outside this Terraform
  state. See [asset hosting](../docs/asset-hosting.md).
