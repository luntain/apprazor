import httplib, urllib

def report(host, revision, test, duration, margin=0.1, server="localhost:5003"):
    params = urllib.urlencode({'revision': revision, 'duration': duration, 'margin': margin})
    conn = httplib.HTTPConnection(server)
    url = '/' + '/'.join(map(urllib.quote, [test, host]))
    conn.request("POST", url, params)
    response = conn.getresponse()
    print response.status, response.reason
    dataLines = response.read().splitlines()
    status = dataLines[0]
    if status == 'PASS':
        return (True, '')
    else:
        return (False, ''.join(dataLines[1:]))

if __name__ == '__main__':
    report('mrlee', '1234M', 'array recalc perf test', 8772)
