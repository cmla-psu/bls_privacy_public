import pytest
import yaml
from accountant import __configreader as creader

def test_config_read_and_save(tmp_path):
    """ Test that the configuration reader stores all information by reading a config file, saving it
    then loading both as yaml objects and comparing them
    """
    theconfig = "tests/testconfig/example.yaml"
    otherconfig = tmp_path.joinpath("test.yaml")
    ingested = creader.FPConfig(theconfig) # ingest into a class
    ingested.save(otherconfig) # save it
    # now load both as yaml and compare
    with open(theconfig, "r") as f1:
        y1 = yaml.safe_load(f1)
    with open(otherconfig, "r") as f2:
        y2 = yaml.safe_load(f2)
    assert y1 == y2
