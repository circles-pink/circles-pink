export const onboardingArgs = {
  lang: {
    defaultValue: "en",
    options: ["en", "de"],
    control: { type: "inline-radio" },
  },
  baseColor: {
    defaultValue: "hotpink",
    options: ["hotpink", "teal", "navy"],
    control: { type: "inline-radio" },
  },
  initState: { control: { disable: true }, table: { disable: true } },
};
