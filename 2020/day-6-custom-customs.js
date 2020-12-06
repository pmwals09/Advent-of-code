const data = require("./data/day-6.json");

const parseDataToGroups = (data) => {
  return data.split("\n\n");
};

const parseGroupsToPeople = (group) => {
  return group.split("\n");
};

const parseGroupQuestions = (group) => {
  const questions = new Set();
  const people = parseGroupsToPeople(group);
  people.forEach((person) => {
    for (let i = 0; i < person.length; i++) {
      questions.add(person[i]);
    }
  });
  return questions;
};

console.log(
  parseDataToGroups(data).reduce((out, group) => {
    return out + parseGroupQuestions(group).size;
  }, 0)
);

const parseGroupAllQuestions = (group) => {
  let allQuestions = Array.from(parseGroupQuestions(group));
  return allQuestions
    .map((question) =>
      parseGroupsToPeople(group).every((person) => person.includes(question))
    )
    .filter((question) => question === true).length;
};

console.log(
  parseDataToGroups(data).reduce((out, group) => {
    return out + parseGroupAllQuestions(group);
  }, 0)
);
