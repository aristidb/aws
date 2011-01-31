function AWSSigner(accessKeyId, secretKey) {
    this.accessKeyId = accessKeyId;
    this.secretKey = secretKey;
}

AWSSigner.prototype.sign = function (params, time, requestInfo) {
    var timeUtc = time.toISO8601();
    params = this.addFields(params, timeUtc);
    params.Signature = this.generateSignature(this.canonicalize(params, requestInfo));
    return params;
}

AWSSigner.prototype.addFields = function (params, time) {
    params.AWSAccessKeyId = this.accessKeyId;
    params.SignatureVersion = this.version;
    params.SignatureMethod = "HmacSHA1";
    params.Timestamp = time;
    return params;
}

AWSSigner.prototype.generateSignature = function (str) {
    return b64_hmac_sha1(this.secretKey, str);
}

AWSV2Signer.prototype = new AWSSigner();

function AWSV2Signer(accessKeyId, secretKey) {
    AWSSigner.call(this, accessKeyId, secretKey);
    this.version = 2;
}

function urlEncode(url) {
    return encodeURIComponent(url)
        .replace(/!/g, '%21')
        .replace(/'/g, '%27')
        .replace(/\(/g, '%28')
        .replace(/\)/g, '%29')
        .replace(/\*/g, '%2A');
}

AWSV2Signer.prototype.canonicalize = function (params, requestInfo) {
    var verb = requestInfo.verb;
    var host = requestInfo.host.toLowerCase();
    var uriPath = requestInfo.uriPath;
    var canonical = verb + "\n" + host + "\n" + uriPath + "\n";
    var sortedKeys = filterAndSortKeys(params, signatureFilter, caseSensitiveComparator);
    var first = true;
    for (var i = 0; i < sortedKeys.length; i++) {
        if (first) {
            first = false;
        } else {
            canonical += "&";
        }
        var key = sortedKeys[i];
        canonical += urlEncode(key);
        if (params[key] !== null) {
            canonical += "=" + urlEncode(params[key]);
        }
    }
    return canonical;
}

function filterAndSortKeys(obj, filter, comparator) {
    var keys = new Array();
    for (var key in obj) {
        if (!filter(key, obj[key])) {
            keys.push(key);
        }
    }
    return keys.sort(comparator);
}

function signatureFilter(key, value) {
    return key === "Signature" || value === null;
}

function caseInsensitiveComparator(a, b) {
    return simpleComparator(a.toLowerCase(), b.toLowerCase());
}

function caseSensitiveComparator(a, b) {
    var length = a.length;
    if (b.length < length) {
        length = b.length;
    }

    for (var i = 0; i < length; i++) {
        var comparison = simpleComparator(a.charCodeAt(i), b.charCodeAt(i));
        if (comparison !== 0) {
            return comparison;
        }
    }

    if (a.length == b.length) {
        return 0;
    }
    if (b.length > a.length) {
        return 1;
    }
    return -1;
}

function simpleComparator(a, b) {
    if (a < b) {
        return -1;
    } else if (a > b) {
        return 1;
    }
    return 0;
}

Date.prototype.toISO8601 = function () {
    return this.getUTCFullYear() + "-" 
    + pad(this.getUTCMonth() + 1) + "-" 
    + pad(this.getUTCDate()) + "T" 
    + pad(this.getUTCHours()) + ":" 
    + pad(this.getUTCMinutes()) + ":" 
    + pad(this.getUTCSeconds()) + ".000Z";
}

function pad(n) {
    return (n < 0 || n > 9 ? "" : "0") + n;
}