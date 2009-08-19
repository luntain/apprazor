import httplib, urllib

def report(host, revision, test, duration, margin=0.1, server="localhost:5003"):
    params = urllib.urlencode({ 'host': host, 'revision': revision,
        'test': test, 'duration': duration, 'margin': margin})
    conn = httplib.HTTPConnection(server)
    conn.request("POST", "/report", params)
    response = conn.getresponse()
    print response.status, response.reason
    dataLines = response.read().splitlines()
    status = dataLines[0]
    if status == 'PASS':
        return (True, '')
    else:
        return (False, dataLines[1])

if __name__ == '__main__':
    report('mrlee', '1234M', 'array recalc perf test', 8772)
