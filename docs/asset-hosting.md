# Assets hosting

Images and rulebook PDFs live in the private S3 bucket `marvel-champions-assets`
in `us-east-1` (AWS account `834069176314`). CloudFront distribution
`E1IAZXVDPSY16L` serves them from `d31bo1u6xkh5bs.cloudfront.net` using origin
access control `E1JX4DWKSKBSD`. S3 public access is blocked; the bucket policy
grants only this distribution `s3:GetObject`. These resources were provisioned
with the AWS CLI and are outside the DigitalOcean Terraform state.

## DNS and HTTPS

Publish these CNAME records in the existing DNS provider:

| Name | Value |
| --- | --- |
| `_c8e51a52de83dab328b09e2f2f16267d.assets.marvelchampions.app` | `_8322ca3b332840514eb4f661176ad4f9.wzccmgtwzk.acm-validations.aws` |
| `assets.marvelchampions.app` | `d31bo1u6xkh5bs.cloudfront.net` |

Keep the validation record for automatic certificate renewal. ACM certificate
`ea8cd360-1d38-475d-bd58-e61f3529f15b` is in `us-east-1`, as required by
CloudFront. After the certificate is issued, attach the hostname and certificate:

```sh
python3 scripts/configure-assets-domain.py
aws cloudfront get-distribution --id E1IAZXVDPSY16L --query Distribution.Status
curl -I https://assets.marvelchampions.app/images/cards/back-player.avif
```

CloudFront changes can take several minutes to deploy. Before the custom domain
is ready, `https://d31bo1u6xkh5bs.cloudfront.net` works as `VITE_ASSET_HOST`.

## Uploads and deployments

```sh
scripts/sync-assets.sh --dryrun
scripts/sync-assets.sh
scripts/deploy.sh
```

Sync uploads only changed images and PDFs and never deletes remote files. It
does not run during each application deployment. Run it before deploying code
that references new assets. Uploaded assets use a one-day cache lifetime; when
replacing an existing file, invalidate its path for immediate updates:

```sh
aws cloudfront create-invalidation --distribution-id E1IAZXVDPSY16L \
  --paths '/images/cards/01001a.avif'
```

`MARVEL_ASSET_BUCKET` overrides the upload bucket. `VITE_ASSET_HOST` overrides
the deployment's CDN origin (default `https://assets.marvelchampions.app`). It
is a build-time Vite setting, passed into the frontend Docker build. The deploy
script verifies the CDN is reachable before starting builds. Both Docker build
context and runtime image exclude `public/images/` and rulebook PDFs; the small
`public/rulebooks/sources.json` stays for source attribution. Local Vite builds
continue to serve local files unless `VITE_ASSET_HOST` is set.
