import httplib, urllib

def report(host, revision, test, duration, server="localhost:8000"):
    params = urllib.urlencode({ 'host': host, 'revision': revision,
        'test': test, 'duration': duration})
    conn = httplib.HTTPConnection(server)
    conn.request("POST", "/report", params)
    response = conn.getresponse()
    print response.status, response.reason
    data = response.read()
    print data
    conn.close()
    return data.strip() == 'PASS'

if __name__ == '__main__':
    report('mrlee', '1234M', 'array recalc perf test', 8772)
