import sys
sys.path.append("../clients")

import httplib, urllib
from python import report
import python
print "PYTHON PATH", python.__file__

class TestEverythingThoroughly(object):

    def testStoringPassAndFail(self):
        # first run
        ret = report('HOST', '1', 'testIt', 1.0)
        assert ret == (True, '')

        # second run - pass
        ret = report('HOST', '1', 'testIt', 0.9)
        assert ret == (True, '')

        # third run - fail
        passed, msg = report('HOST', '2', 'testIt', 100.0)
        assert not passed
        assert "0.9" in msg


    def testRatcheting(self):
        report('HOST', '1', 'testRatcheting', 10)

        passed, _ = report('HOST', '1', 'testRatcheting', 11.0001)
        assert not passed


    def testMargin(self):
        report('HOST', '1', 'testCustomMargin', duration=10)

        passed, _ = report('HOST', '2', 'testCustomMargin', duration=14.9999, margin=0.5)
        assert passed

        passed, _ = report('HOST', '2', 'testCustomMargin', duration=15.00001, margin=0.5)
        assert not passed


    def testRemove(self):
        report('HOST', '1', 'testRemove', duration=10)
        self.removeResult('HOST', '1', 'testRemove', duration=10)
        passed, _ = report('HOST', '1', 'testRemove', duration=10)
        assert passed


    def removeResult(self, host, revision, test, duration):
        params = urllib.urlencode({'revision': revision, 'duration': duration})
        conn = httplib.HTTPConnection("localhost:5003")
        conn.request("POST", "/%s/%s/" % (test, host), params)
        response = conn.getresponse()
