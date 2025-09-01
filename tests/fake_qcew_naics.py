import numpy as np
import scipy.stats as ss
import pandas as pd
import sys
import os
sys.path.append(".")
from readers import QCEWMicroDataReader


def generate(n=400_000, seed=None):
    prng = np.random.default_rng(seed)
    state = "30"
    ein = prng.choice(list(range(1000100, 1000100 + 3 * n + 1)), size=n, replace=False)
    # ein = prng.choice(list(range(1000100, 1000101)), size=n, replace=True)

    ui = prng.choice(list(range(1, n)), size=n, replace=True)
    ru_num = prng.choice(list(range(1, 55120)), size=n, replace=True)

    cnty_choices = np.append(prng.choice([str(x).zfill(3) for x in range(1, 301)], size=80, replace=False),
                             ["995", "998", "999"])
    # cnty_choices = [102,224]
    cnty = prng.choice(cnty_choices, size=n, replace=True)

    m1emp = np.round(np.exp(ss.poisson.rvs(mu=1.4, size=n, random_state=prng))).astype(int) - 1

    m2emp = np.round((m1emp * np.maximum(1, ss.norm.rvs(size=n, loc=0, scale=0.5, random_state=prng))) * (
            1 + ss.norm.rvs(size=n, loc=0.005, scale=0.5, random_state=prng))).astype(int)
    m2emp = np.maximum(0, m2emp)

    m3emp = np.round((m2emp * np.maximum(1, ss.norm.rvs(size=n, loc=0, scale=0.5, random_state=prng))) * (
            1 + ss.norm.rvs(size=n, loc=0.005, scale=0.5, random_state=prng))).astype(int)
    m3emp = np.maximum(0, m3emp)

    avemp = (m1emp + m2emp + m3emp) / 3.0

    wage = (ss.norm.rvs(size=n, loc=4000, scale=2000, random_state=prng)
            + 10000 * ss.poisson.rvs(size=n, mu=0.15, random_state=prng)) * (
                   avemp + ss.binom.rvs(size=n, n=1, p=0.005, random_state=prng))
    wage = np.maximum(0, wage)

    contrib = 0
    txwage = 0
    can_agg = 'Y'
    own = 5
    rectype = 'C'
    primary_key = list(range(n))
    nc = ['81', '52', '23', '45', '42', '71', '72', '55', '22', '44', '33', '32', '49', '00', '51', '21', '31', '54',
          '48', '61', '56', '53', '11', '62', '92', '99']
    naics = [i + j for i, j in
             zip(prng.choice(nc, size=n), prng.choice([str(x) for x in list(range(1110, 9999))], size=n))]
    # naics_choices = ['548207','522511']
    # naics = prng.choice(naics_choices, size=n, replace=True)

    #naicsprefix = [a[0:2] for a in naics], prefix should be a derived field
    data = {QCEWMicroDataReader.STATE: state,
            QCEWMicroDataReader._CONTRIB: contrib,
            QCEWMicroDataReader._TXWAGE: txwage,
            QCEWMicroDataReader._CAN_AGG: can_agg,
            QCEWMicroDataReader._RECTYPE: rectype,
            QCEWMicroDataReader.OWN: own,
            QCEWMicroDataReader.PRIMARY_KEY: primary_key,
            QCEWMicroDataReader.YEAR: '2023',
            QCEWMicroDataReader.QTR: '1',
            QCEWMicroDataReader.SECTOR: '0',
            QCEWMicroDataReader.SUPERSECTOR: '0',
            QCEWMicroDataReader._EIN: ein,
            QCEWMicroDataReader.NAICS: naics,
            #QCEWMicroDataReader.NAICSPREFIX: naicsprefix,
            QCEWMicroDataReader._UI: ui,
            QCEWMicroDataReader._RUNUM: ru_num,
            QCEWMicroDataReader.COUNTY: cnty,
            QCEWMicroDataReader.M1EMP: m1emp,
            QCEWMicroDataReader.M2EMP: m2emp,
            QCEWMicroDataReader.M3EMP: m3emp,
            QCEWMicroDataReader.WAGE: wage
            }
    return pd.DataFrame(data)


def save(df, name, directory):
    path = str(directory)
    if not os.path.exists(path):
        os.makedirs(path)
    df.to_csv(directory + '/' + str(name), index=False)


def load(name):
    return pd.read_csv(name, index_col=0)


def main():
    file = ''
    rows = 400000
    directory = ''

    for idx, argv in enumerate(sys.argv):
        if '--directory' in argv:
            directory = argv.partition('=')[2]
        if '--file' in argv:
            file = argv.partition('=')[2]
        if '--rows' in argv:
            rows = argv.partition('=')[2]

    df = generate(n=int(rows), seed=None)
    save(df=df, name=file, directory=directory)


if __name__ == "__main__":
    main()
