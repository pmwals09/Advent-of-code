const data = require("./data/day-4.json");

const parseDataIntoPassports = (data) => {
  return data.split("\n\n").map((passport) => passport.replace(/\n/g, " "));
};

const requiredFields = [
  "byr",
  "iyr",
  "eyr",
  "hgt",
  "hcl",
  "ecl",
  "pid",
];

const validatePassport = (passport, requiredFields) => {
  return requiredFields.every((requiredField) =>
    passport.includes(requiredField + ":")
  );
};

console.log(
  parseDataIntoPassports(data)
    .map((ea) => validatePassport(ea, requiredFields))
    .filter((ea) => ea === true).length
);

const validatePassportData = (passport) => {
  return (
    validateBirthYear(passport) &&
    validateIssueYear(passport) &&
    validateExpirationYear(passport) &&
    validateHeight(passport) &&
    validateHairColor(passport) &&
    validateEyeColor(passport) &&
    validatePID(passport)
  );
};

const validateBirthYear = (passport) => {
  const byrField = passport.match(/byr:(\d{4})(\s|$)/);
  if (byrField === null) return false;
  const byrVal = parseInt(byrField[1]);
  return byrVal >= 1920 && byrVal <= 2002;
};

const validateIssueYear = (passport) => {
  const iyrField = passport.match(/iyr:(\d{4})(\s|$)/);
  if (iyrField === null) return false;
  const iyrVal = parseInt(iyrField[1]);
  return iyrVal >= 2010 && iyrVal <= 2020;
};

const validateExpirationYear = (passport) => {
  const eyrField = passport.match(/eyr:(\d{4})(\s|$)/);
  if (eyrField === null) return false;
  const eyrVal = parseInt(eyrField[1]);
  return eyrVal >= 2020 && eyrVal <= 2030;
};

const validateHeight = (passport) => {
  const hgtField = passport.match(/hgt:(\d+)(cm|in)(\s|$)/);
  if (hgtField === null) return false;
  if (hgtField[2] === "cm")
    return parseInt(hgtField[1]) >= 150 && parseInt(hgtField[1]) <= 193;
  if (hgtField[2] === "in")
    return parseInt(hgtField[1]) >= 59 && parseInt(hgtField[1]) <= 76;
  return false;
};

const validateHairColor = (passport) => {
  const hclField = passport.match(/hcl:#[0-9a-f]{6}(\s|$)/);
  return hclField !== null;
};

const validateEyeColor = (passport) => {
  const eclField = passport.match(/ecl:(amb|blu|brn|gry|grn|hzl|oth)(\s|$)/);
  return eclField !== null;
};

const validatePID = (passport) => {
  const pidField = passport.match(/pid:\d{9}(\s|$)/);
  return pidField !== null;
};

console.log(
  parseDataIntoPassports(data)
    .map((ea) => validatePassportData(ea, requiredFields))
    .filter((ea) => ea === true).length
);