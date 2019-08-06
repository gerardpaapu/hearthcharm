exports.parseMessage = function (src) {
    const pattern = /\{([a-z ,-']+)\}/mgi;
    const result = [];
    let match;

    while ((match = pattern.exec(src)) != null) {
        result.push(match[1]); 
    }

    return result;
};
