def getpf(pf):
    if 'tenancies' in globals():
        for tenant in tenancies:
            if tenancies[tenant].get('tenanttype', '') == 'Super' or len(tenancies) == 1:
                return '{0}_{1}.pf'.format(pf, tenant)
    return '{0}.pf'.format(pf)