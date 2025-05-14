<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRfindCriteriaEbEntityLink`

## 1. Overview:

### Objective and Problem Solved:
The `FRfindCriteriaEbEntityLink` code snippet defines a form component that allows users to filter and search for entities based on specific criteria such as entity type, party type, and other related attributes. It provides a user interface for selecting these criteria and generates the corresponding search parameters. This is particularly useful in applications where users need to query or filter data related to entities in a structured and user-friendly manner.

### Technologies Used:
- **Delphi (Object Pascal):** The code is written in Delphi, utilizing its VCL (Visual Component Library) for UI components.
- **Third-party Libraries:** Includes components like `TcxImageComboBox`, `TsLabel`, and `TFRAMEFindEditSOA` for enhanced UI and functionality.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types:**
  - Labels (`TsLabel`): Display descriptive text for form fields.
  - Combo Boxes (`TcxImageComboBox`): Dropdowns for selecting entity types, party types, and other attributes.
  - Text Edit (`TcxTextEdit`): Input field for entering specific entity details.
  - Custom Frame (`TFRAMEFindEditSOA`): A specialized component for entity search.
- **Form Actions and Effects:**
  - Dropdown selection changes trigger events to update the form state or prepare search criteria.
  - Text input allows users to specify additional search parameters.
  - The form generates search criteria based on user inputs.

---

## 2. Functionality Description:

### User/Software Actions:
- Users can select entity types, party types, and other attributes using dropdowns.
- Users can input specific entity details in the text field.
- The form generates search criteria based on the selected and entered values.

### Main Components:
- **Labels (`TsLabel`):** Provide context for each input field.
- **Combo Boxes (`TcxImageComboBox`):** Allow users to select predefined options.
- **Text Edit (`TcxTextEdit`):** Enables manual input of entity details.
- **Custom Frame (`TFRAMEFindEditSOA`):** Facilitates advanced entity search.

### Pseudo-code for Actions and Events:
- `OnChange` event of a combo box: `if combo box value changed then update related fields`.
- `OnClick` event of a combo box: `if combo box clicked then display dropdown options`.
- `OnExit` event of a combo box: `if combo box loses focus then validate selection`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization:**
   - The form is initialized with default values and styles in the `Create` constructor.
   - Combo boxes are populated with predefined options.
   - Find dialogs are prepared for advanced search functionality.

2. **User Interaction:**
   - Users select options from combo boxes or input text in the text field.
   - Events are triggered to update the form state or validate inputs.

3. **Search Criteria Generation:**
   - The `GetCriteriaValues` function generates search criteria based on user inputs.

### Data Requirements:
- Users must select or input:
  - Entity type.
  - Party type.
  - Specific entity details (optional).

---

## 4. Business Rules:

### Actions and Preconditions:
- **Dropdown Selection:**
  - Preconditions: Dropdown must be populated with options.
  - Action: Updates related fields or prepares search criteria.
- **Search Criteria Generation:**
  - Preconditions: At least one valid input or selection must be made.

### Available Filters:
- **Entity Type:** Options include "All," "Customer," etc.
- **Party Type:** Options are dynamically populated.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- **Entity Type:** Default to "All."
- **Party Type:** Default to the first option in the dropdown.

### Field Validation and Conditions:
- **Entity Type:** Must be a valid selection from the dropdown.
- **Party Type:** Must be a valid selection from the dropdown.
- **Entity Details:** Optional but must be valid if provided.

---

## 5. Main Functions:

### Functions:
1. **`Create`:** Initializes the form, prepares find dialogs, and sets default styles.
2. **`GetCriteriaValues`:** Generates search criteria based on user inputs.
3. **`m_PrepareFindDialogs`:** Prepares find dialogs for advanced search.
4. **`m_PrepareCombos`:** Populates combo boxes with predefined options.

---

## 6. API Service Consumption:

- No external API calls are explicitly defined in the provided code.

---

## 7. Conditional Fields (Form Logic):

- The `FRAMEFindEntity` field is enabled only when a valid entity type is selected.

---

## 8. Dependencies:

### External Libraries:
- **VCL Components:** For UI elements.
- **Third-party Components:** `TcxImageComboBox`, `TsLabel`, `TFRAMEFindEditSOA`.

### Custom Components:
- **`TFRAMEFindEditSOA`:** A custom frame for advanced entity search.

---

## 9. Fields and Validations Listing:

### Fields:
1. **Entity Type (`ICBOentity_tp`):**
   - Type: Dropdown.
   - Required: Yes.
   - Default: "All."
2. **Party Type (`ICBOeb_Party_Tp`):**
   - Type: Dropdown.
   - Required: Yes.
   - Default: First option.
3. **Entity Details (`FRAMEFindEntity`):**
   - Type: Text.
   - Required: No.

### Mapping:
- Not explicitly defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Form] --> [User Inputs Data] --> [Generate Criteria] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Form: Selects options or inputs text
Form --> Internal Logic: Validates inputs
Internal Logic --> Form: Updates state or generates criteria
```

### Code Snippets:
```pascal
procedure TFRAMEfindCriteriaEbEntityLink.ICBOentity_tpPropertiesEditValueChanged(Sender: TObject);
begin
  // Update related fields based on the selected entity type
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

- **Initialization:**
  - `m_PrepareFindDialogs` and `m_PrepareCombos` are called to set up the form.
- **Search Criteria:**
  - `GetCriteriaValues` generates criteria based on user inputs.

---

## 12. Conclusion:

The `FRfindCriteriaEbEntityLink` form provides a structured way to filter and search for entities based on user-defined criteria. Its strengths include a user-friendly interface and dynamic criteria generation. However, error handling and validation could be more explicitly defined.

---

## 13. Short Summary:

The `FRfindCriteriaEbEntityLink` form enables users to filter and search for entities using dropdowns and text inputs. It dynamically generates search criteria based on user inputs, making it a valuable component for data querying in Delphi applications.#### **FRfindCriteriaEbEntityLink.pas**

```
unit FRfindCriteriaEbEntityLink;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRfindCriteria, sFrameAdapter, cxGraphics, cxControls,
  cxContainer, cxEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit,
  cxImageComboBox, cxDBEdit, StdCtrls, Mask, DBCtrls, sDBEdit,
  kneFRFindEditSOA, sLabel, kneFindDialog, kneConfigObjects, DMskin;

type
  TFRAMEfindCriteriaEbEntityLink = class(TFRAMEfindCriteria)
    LBLeb_party_tp: TsLabel;
    LBLeb_Party: TsLabel;
    LBLentity_tp: TsLabel;
    LBLentity_cd: TsLabel;
    LBLeb_entity_type: TsLabel;
    FRAMEFindEntity: TFRAMEFindEditSOA;
    ICBOentity_tp: TcxImageComboBox;
    ICBOeb_entity_type: TcxImageComboBox;
    EDTeb_Party: TcxTextEdit;
    ICBOeb_Party_Tp: TcxImageComboBox;
    procedure ICBOentity_tpPropertiesEditValueChanged(Sender: TObject);
    procedure ICBOentity_tpExit(Sender: TObject);
    procedure ICBOentity_tpPropertiesCloseUp(Sender: TObject);
    procedure ICBOentity_tpEnter(Sender: TObject);
    procedure ICBOentity_tpClick(Sender: TObject);
  private
    { Private declarations }
    FindDlgAgent : TFORMkneFindDialog;
    FindDlgConsignee : TFORMkneFindDialog;
    FindDlgCustomer : TFORMkneFindDialog;
    procedure m_DestroyFindDialogs;
    procedure m_PrepareFindDialogs;
    procedure m_SetFindEntity(const p_EntityType: string);
    procedure m_PrepareCombos;
//  protected
//    function GetCriteriaValues: TArrayOfFieldCriteria;  override;
  public
    { Public declarations }
    constructor Create(Aowner : TComponent); override;
    destructor Destroy; override;
    function GetCriteriaValues: TArrayOfFieldCriteria;  override;
  end;

var
  FRAMEfindCriteriaEbEntityLink: TFRAMEfindCriteriaEbEntityLink;

implementation


uses kneDialogFactory,kneFGGenericUtils, AgentServiceUtils, kneFGFindUtils, EbEntityLinkServiceUtils;
{$R *.dfm}

constructor TFRAMEfindCriteriaEbEntityLink.Create(Aowner: TComponent);
begin
  inherited;
  FindDlgCustomer := nil;
  FindDlgConsignee := nil;
  FindDlgAgent := nil;

  m_PrepareFindDialogs;
  m_PrepareCombos;


  // Somente para inicializar com algo.
  ICBOentity_tp.ItemIndex := 0;

  ICBOentity_tp.Style.StyleController := DMODskin.cxEditStyles1;
  ICBOeb_entity_type.Style.StyleController := DMODskin.cxEditStyles1;
  ICBOeb_Party_Tp.Style.StyleController := DMODskin.cxEditStyles1;
end;

function TFRAMEfindCriteriaEbEntityLink.GetCriteriaValues: TArrayOfFieldCriteria;
var lv_temp : string;
begin
  //Result := inherited;
  // retorna nil pq n�o usa FieldCriteria para selec��o
  Result := nil;

  if ICBOentity_tp.ItemIndex>0 then
  begin
    lv_temp := ICBOentity_tp.Properties.Items[ICBOentity_tp.ItemIndex].Value;
    if lv_temp<>'' then
    begin
      addCriteria(result, 'AND', 'entityTp','=', lv_temp);
      if FRAMEFindEntity.Text<>'' then
        addCriteria(result, 'AND', 'entity','=', FRAMEFindEntity.Text);
    end;
  end;

  if ICBOeb_entity_type.ItemIndex>=0 then
  begin
    lv_temp := ICBOeb_entity_type.Properties.Items[ICBOeb_entity_type.ItemIndex].Value;
    if lv_temp<>'' then
      addCriteria(result, 'AND', 'ebEntityType','=', lv_temp);
  end;

  if ICBOeb_Party_Tp.ItemIndex>0 then
```

#### **FRfindCriteriaEbEntityLink.dfm**

```
inherited FRAMEfindCriteriaEbEntityLink: TFRAMEfindCriteriaEbEntityLink
  Width = 598
  Height = 137
  object LBLeb_party_tp: TsLabel [0]
    Left = 216
    Top = 65
    Width = 69
    Height = 13
    Caption = 'EbParty Type:'
    FocusControl = ICBOeb_Party_Tp
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLeb_Party: TsLabel [1]
    Left = 8
    Top = 91
    Width = 42
    Height = 13
    Caption = 'EbParty:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLentity_tp: TsLabel [2]
    Left = 8
    Top = 13
    Width = 57
    Height = 13
    Caption = 'Entity type:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLentity_cd: TsLabel [3]
    Left = 8
    Top = 39
    Width = 32
    Height = 13
    Caption = 'Entity:'
    FocusControl = FRAMEFindEntity
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLeb_entity_type: TsLabel [4]
    Left = 8
    Top = 65
    Width = 69
    Height = 13
    Caption = 'EbEntity type:'
    FocusControl = ICBOeb_entity_type
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  inline FRAMEFindEntity: TFRAMEFindEditSOA [5]
    Left = 86
    Top = 34
    Width = 486
    Height = 21
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 1
  end
  object ICBOentity_tp: TcxImageComboBox
    Left = 86
    Top = 8
    Properties.Items = <
      item
        Description = 'All'
        ImageIndex = 0
        Value = ''
      end
      item
        Description = 'Customer'
        Value = 'CUST'
```
<!-- tabs:end -->

