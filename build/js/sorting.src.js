
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"actvars","title":"Subjects","type":"Variables","suggested":["nominal","ordinal"],"permitted":["factor"]},{"name":"individus","title":"Stimuli Labels","type":"Variable","suggested":["nominal"],"permitted":["factor"]},{"name":"quantisup","title":"Quantitative Supplementary Variables","type":"Variables","suggested":["continuous"],"permitted":["numeric"]},{"name":"qualisup","title":"Supplementary Subjects","type":"Variables","suggested":["nominal","ordinal"],"permitted":["factor"]},{"name":"nFactors","title":"Number of factors","type":"Integer","default":3},{"name":"abs","title":"X-axis","type":"Integer","default":1},{"name":"ord","title":"Y-axis","type":"Integer","default":2},{"name":"varmodqualisup","title":"Supplementary categories","type":"Bool","default":true},{"name":"varmodvar","title":"Active categories","type":"Bool","default":true},{"name":"quantimod","title":"Quantitative supplementary variables","type":"Bool","default":false},{"name":"proba","title":"Significance threshold (%)","type":"Number","default":5},{"name":"indcoord","title":"Coordinates","type":"Bool","default":false},{"name":"indcontrib","title":"Contributions","type":"Bool","default":false},{"name":"indcos","title":"Cosine","type":"Bool","default":false},{"name":"varcoord","title":"Coordinates","type":"Bool","default":false},{"name":"varcontrib","title":"Contributions","type":"Bool","default":false},{"name":"varcos","title":"Cosine","type":"Bool","default":false},{"name":"ventil","title":"Ventilation level (%)","type":"Number","default":5},{"name":"modality","title":"Selection of categories","type":"String","default":"cos2 10"},{"name":"leveltext","title":"Level description (%)","type":"Number","default":5}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Analysis of Sorting Data",
    jus: "3.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.VariableSupplier,
			typeName: 'VariableSupplier',
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Stimuli Labels",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "individus",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Subjects",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "actvars",
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Quantitative Supplementary Variables",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "quantisup",
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Supplementary Subjects",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "qualisup",
							isTarget: true
						}
					]
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Random Assignment for Rare Categories",
					stretchFactor: 1,
					controls: [
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
							name: "ventil",
							format: FormatDef.number
						}
					]
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.CollapseBox,
					typeName: 'CollapseBox',
					label: "Graphic Options",
					collapsed: true,
					controls: [
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Components to Plot",
							controls: [
								{
									type: DefaultControls.Label,
									typeName: 'Label',
									label: "",
									cell: {"column":0,"row":0},
									stretchFactor: 1,
									controls: [
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "abs",
											format: FormatDef.number
										}
									]
								},
								{
									type: DefaultControls.Label,
									typeName: 'Label',
									label: "",
									cell: {"column":1,"row":0},
									stretchFactor: 1,
									controls: [
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "ord",
											format: FormatDef.number
										}
									]
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Categories to Plot",
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "varmodvar"
								},
								{
									type: DefaultControls.TextBox,
									typeName: 'TextBox',
									name: "modality",
									format: FormatDef.string
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "varmodqualisup"
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Quantitative Supplementary Variables",
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "quantimod"
								}
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.CollapseBox,
					typeName: 'CollapseBox',
					label: "Numerical Indicators",
					collapsed: true,
					controls: [
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Automatic Description of the Axis",
							controls: [
								{
									type: DefaultControls.Label,
									typeName: 'Label',
									label: "",
									cell: {"column":0,"row":0},
									stretchFactor: 1,
									controls: [
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "proba",
											format: FormatDef.number
										}
									]
								},
								{
									type: DefaultControls.Label,
									typeName: 'Label',
									label: "",
									cell: {"column":1,"row":0},
									stretchFactor: 1,
									controls: [
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "nFactors",
											format: FormatDef.number
										}
									]
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Automatic Description of the Stimuli",
							controls: [
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									margin: "large",
									controls: [
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "leveltext",
											format: FormatDef.number
										}
									]
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Individual Table Options",
							controls: [
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									margin: "large",
									stretchFactor: 1,
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "indcoord"
										},
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "indcontrib"
										},
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "indcos"
										}
									]
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Variable Table Options",
							controls: [
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									margin: "large",
									stretchFactor: 1,
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "varcoord"
										},
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "varcontrib"
										},
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "varcos"
										}
									]
								}
							]
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
