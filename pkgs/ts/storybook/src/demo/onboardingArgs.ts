export const onboardingArgs = {
  lang: {
    defaultValue: "en",
    options: ["en", "de"],
    control: { type: "inline-radio" },
  },
  initState: { control: { disable: true }, table: { disable: true } },
};
