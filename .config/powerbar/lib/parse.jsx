const parse = (data) => {
  try {
    return JSON.parse(data)
  } catch (e) {
    return undefined;
  }
};

export default parse;
