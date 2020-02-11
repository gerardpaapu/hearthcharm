exports.parseMessageImpl = function (src) {
  return function (Nothing) {
    return function (Battlegrounds) {
      return function (Constructed) {
        const pattern = /\{(c#|b#)?([a-z0-9 :!,'-]+)\}/mgi;
        const result = [];
        let match;

        while ((match = pattern.exec(src)) != null) {
          switch (match[1]) {
            case 'c#':
              result.push(Constructed(match[2]));
              break;
              
            case 'b#':
              result.push(Battlegrounds(match[2]));
              break;

            default:
              result.push(Nothing(match[2]));
              break;
          }
        }


        return result;
      };
    };
  };
};

exports.callbackWaitsForEmptyLoop = function (b) {
  return function (ctx) {
    return function () {
      return ctx.callbackWaitsForEmptyLoop = b;
    };
  };
};
