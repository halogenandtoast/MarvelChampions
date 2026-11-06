#!/usr/bin/env python3
"""Attach the assets hostname after its ACM DNS validation record is published."""
import json
import subprocess
import tempfile

DISTRIBUTION = 'E1IAZXVDPSY16L'
CERTIFICATE = 'arn:aws:acm:us-east-1:834069176314:certificate/ea8cd360-1d38-475d-bd58-e61f3529f15b'
DOMAIN = 'assets.marvelchampions.app'


def aws(*args):
    return json.loads(subprocess.check_output(['aws', *args, '--output', 'json']))


def main():
    certificate = aws('acm', 'describe-certificate', '--region', 'us-east-1',
                      '--certificate-arn', CERTIFICATE)['Certificate']
    if certificate['Status'] != 'ISSUED':
        print('Certificate status:', certificate['Status'])
        for option in certificate['DomainValidationOptions']:
            print('Publish DNS record:', json.dumps(option.get('ResourceRecord', {})))
        raise SystemExit('Run this script again after ACM issues the certificate.')

    current = aws('cloudfront', 'get-distribution-config', '--id', DISTRIBUTION)
    config = current['DistributionConfig']
    aliases = config.get('Aliases', {}).get('Items', [])
    if DOMAIN in aliases and config['ViewerCertificate'].get('ACMCertificateArn') == CERTIFICATE:
        print('Assets hostname is already configured.')
        return
    aliases = list(dict.fromkeys([*aliases, DOMAIN]))
    config['Aliases'] = {'Quantity': len(aliases), 'Items': aliases}
    config['ViewerCertificate'] = {
        'ACMCertificateArn': CERTIFICATE,
        'SSLSupportMethod': 'sni-only',
        'MinimumProtocolVersion': 'TLSv1.2_2021',
    }
    with tempfile.NamedTemporaryFile(mode='w+', suffix='.json') as config_file:
        json.dump(config, config_file)
        config_file.flush()
        result = aws('cloudfront', 'update-distribution', '--id', DISTRIBUTION,
                     '--if-match', current['ETag'], '--distribution-config', 'file://' + config_file.name)
    print('Distribution:', result['Distribution']['DomainName'])
    print('Status:', result['Distribution']['Status'])
    print('DNS: CNAME', DOMAIN, '->', result['Distribution']['DomainName'])


if __name__ == '__main__':
    main()
