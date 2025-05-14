<!-- tabs:start -->

#### **Documentation**

# Documentation for `EdocsCheckListRules`

## 1. Overview:

### Objective:
The `EdocsCheckListRules` code snippet defines a form-based application for managing and displaying rules related to document checklists. It provides a user interface to view, edit, and manage criteria and documents associated with these rules. The form is designed to handle master-detail relationships between data entities and allows users to interact with the data through a tabbed interface.

### Technologies Used:
- **Delphi VCL Framework**: Used for building the graphical user interface.
- **Third-party Components**: Includes components like `TsScrollBox`, `TsPageControl`, `TcxGrid`, and others for enhanced UI/UX.
- **Object-Oriented Programming (OOP)**: The form and its components are implemented using OOP principles.

### Form Type:
This is a **form-based application** with the following elements:
- **Form Elements**:
  - `PNLdata`: A scrollable panel for displaying data.
  - `PGCdetails`: A tabbed control with two tabs: "Criteria" and "Documents".
  - `FRAMEdocsCheckRules1`: A frame for managing document check rules.
  - `FRAMEdocsCheckRulesCriteria1`: A frame for managing criteria.
  - `FRAMEdocsCheckRulesDocs1`: A frame for managing documents.
- **Form Actions**:
  - Switching between tabs to view/edit criteria and documents.
  - Loading and displaying data in a master-detail relationship.

---

## 2. Functionality Description:

### User/Software Actions:
- View and edit document check rules.
- Navigate between "Criteria" and "Documents" tabs to manage respective data.
- Load data from a master-detail relationship and display it in grids.

### Main Components:
- **`PNLdata`**: A scrollable container for the main content.
- **`PGCdetails`**: A tabbed control for switching between "Criteria" and "Documents".
- **`FRAMEdocsCheckRules1`**: Displays general information about document check rules.
- **`FRAMEdocsCheckRulesCriteria1`**: Displays and manages criteria in a grid.
- **`FRAMEdocsCheckRulesDocs1`**: Displays and manages documents in a grid.

### Pseudo-code for Actions and Events:
- **Form Creation**:
  ```
  if form is created then
    set active tab to "Criteria"
  ```
- **Data Loading**:
  ```
  if m_getData is called then
    set master-detail relationships for criteria and documents
    allow access to inactive records
    load data into the form
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is created, and the "Criteria" tab is set as the active tab.
2. **Data Loading**:
   - The `m_getData` method is called to load data into the form.
   - Master-detail relationships are established for criteria and documents.
   - Data is displayed in the respective grids.

### Required User Input:
- Users must interact with the tabs and grids to view or edit data.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Switch Tabs**: Users can switch between "Criteria" and "Documents" tabs without preconditions.
- **Data Loading**: Data is loaded automatically when the form is initialized.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- No error messages are explicitly defined in the code.

### Default Field Values:
- The "Criteria" tab is set as the default active tab.

### Field Validation and Conditions:
- No explicit field validations or conditions are defined in the code.

---

## 5. Main Functions:

### Functions:
1. **`m_CreateFormEdit`**:
   - Creates and initializes the form.
   - **Business Logic**: Ensures the form is properly instantiated and ready for use.
2. **`m_getData`**:
   - Loads data into the form and establishes master-detail relationships.
   - **Business Logic**: Ensures data is displayed correctly and inactive records are accessible.

---

## 6. API Service Consumption:

- No external API calls are defined in the code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **`kneUtils`**: Utility functions for handling master-detail relationships.
- **Third-party UI components**: Includes `TsScrollBox`, `TsPageControl`, `TcxGrid`, etc.

### Custom Components:
- **`TFRAMEdocsCheckRules`**: Custom frame for managing document check rules.
- **`TFRAMEdocsCheckRulesCriteria`**: Custom frame for managing criteria.
- **`TFRAMEdocsCheckRulesDocs`**: Custom frame for managing documents.

---

## 9. Fields and Validations Listing:

### Fields:
- **Criteria Grid**:
  - Data source: `FRAMEdocsCheckRulesCriteria1.DStable`.
  - No explicit validations are defined.
- **Documents Grid**:
  - Data source: `FRAMEdocsCheckRulesDocs1.DStable`.
  - No explicit validations are defined.

### Mapping:
- Displayed values are mapped to the respective data sources (`DStable`).

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Form Initialization] --> [Set Active Tab to "Criteria"] --> [Load Data] --> [Display Data in Grids]
```

### Sequence Diagram:
```plaintext
User --> Form: Open Form
Form --> Data Source: Load Data
Data Source --> Form: Return Data
Form --> User: Display Data
```

### Code Snippets:
```delphi
procedure TFORMEdocsCheckListRules.FormCreate(Sender: TObject);
begin
  inherited;
  PGCdetails.ActivePageIndex := 0; // Set default tab to "Criteria"
end;
```

### Screenshots:
HTML representation of the form:
```html
<div style="font-family: Verdana; width: 784px; height: 562px; border: 1px solid #000;">
  <div style="height: 41px; background-color: #f0f0f0;">Docs Check List Rules</div>
  <div style="overflow-y: scroll; height: 483px;">
    <div style="height: 120px; background-color: #e0e0e0;">Document Check Rules Frame</div>
    <div style="height: 359px;">
      <div style="height: 50%; background-color: #d0d0d0;">Criteria Tab</div>
      <div style="height: 50%; background-color: #c0c0c0;">Documents Tab</div>
    </div>
  </div>
</div>
```

---

## 11. Important Comments in the Code:

- **`m_getData`**:
  - Establishes master-detail relationships and allows access to inactive records.
- **`FormCreate`**:
  - Sets the default active tab to "Criteria".

---

## 12. Conclusion:

The `EdocsCheckListRules` form provides a structured interface for managing document checklist rules. It effectively handles master-detail relationships and displays data in a user-friendly manner. However, the code lacks explicit error handling, field validations, and filters, which could be added to enhance functionality.

---

## 13. Short Summary:

The `EdocsCheckListRules` form manages document checklist rules with a tabbed interface for criteria and documents. It supports master-detail relationships and displays data in grids, but lacks explicit error handling and field validations.#### **EdocsCheckListRules.pas**

```
unit EdocsCheckListRules;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBEdit, ActnList, ImgList, knePrivileges, StdCtrls, Buttons,
  sBitBtn, sSpeedButton, ToolWin, ComCtrls, acCoolBar, ExtCtrls, sPanel,
  kneEnterAsTab, kneFRGridEditSOA, kneFREditSOA,
  kneFRCtrlEditSOA, FRdocsCheckRules, sScrollBox, sPageControl,
  FRdocsCheckRulesDocs, FRdocsCheckRulesCriteria;

type
  TFORMEdocsCheckListRules = class(TFORMkneBaseEdit)
    PNLdata: TsScrollBox;
    FRAMEdocsCheckRules1: TFRAMEdocsCheckRules;
    PGCdetails: TsPageControl;
    TSHcriteria: TsTabSheet;
    TSHdocuments: TsTabSheet;
    FRAMEdocsCheckRulesDocs1: TFRAMEdocsCheckRulesDocs;
    FRAMEdocsCheckRulesCriteria1: TFRAMEdocsCheckRulesCriteria;
    procedure FormCreate(Sender: TObject);

  protected
    procedure m_getData; override;

  private
    { Private declarations }
  public
    { Public declarations }
    class function m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit; override;
  end;

var
  FORMEdocsCheckListRules: TFORMEdocsCheckListRules;

implementation

uses
  kneUtils;

{$R *.dfm}

{ TFORMEdocsCheckListRules }

class function TFORMEdocsCheckListRules.m_CreateFormEdit(
  const AOwner: TComponent): TFORMkneBaseEdit;
begin
  Result := TFORMEdocsCheckListRules.Create(Application);
end;

procedure TFORMEdocsCheckListRules.m_getData;
var
  lv_MasterFrame: TFRAMEBaseEditSOA;
begin
  Screen.Cursor := crHourGlass;

  lv_MasterFrame := TFRAMEBaseEditSOA(kneUtils.TkneGeneric.fg_GetMasterFrame(Self));

  // setup das rela��es master-detail
  FRAMEdocsCheckRulesCriteria1.MasterSource := lv_MasterFrame.DStable;
  FRAMEdocsCheckRulesDocs1.MasterSource     := lv_MasterFrame.DStable;

  lv_MasterFrame.ServiceParams.ShowInactives := True; // para permitir aceder aos registos inativos

  inherited m_getData;

end;

procedure TFORMEdocsCheckListRules.FormCreate(Sender: TObject);
begin
  inherited;
  PGCdetails.ActivePageIndex := 0;
end;

end.
```

#### **EdocsCheckListRules.dfm**

```
inherited FORMEdocsCheckListRules: TFORMEdocsCheckListRules
  Height = 562
  Caption = 'Docs Check List Rules'
  Font.Name = 'Verdana'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object PNLdata: TsScrollBox [2]
    Left = 0
    Top = 41
    Width = 784
    Height = 483
    Align = alClient
    TabOrder = 1
    SkinData.SkinSection = 'PANEL_LOW'
    inline FRAMEdocsCheckRules1: TFRAMEdocsCheckRules
      Left = 0
      Top = 0
      Width = 780
      Height = 120
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      inherited PNLfooter: TsPanel
        Top = 86
        Width = 780
      end
      inherited FRAMEstatusInfo1: TFRAMEstatusInfo
        Top = 44
        Width = 780
        inherited GRPstatus: TsGroupBox
          Width = 780
          inherited DBTXTlastUpd: TsDBText
            Font.Color = 5059883
            DataSource = FRAMEdocsCheckRules1.DStable
          end
          inherited DBTXTupdBy: TsDBText
            Font.Color = 5059883
            DataSource = FRAMEdocsCheckRules1.DStable
          end
          inherited ICBOstat: TcxDBImageComboBox
            DataBinding.DataSource = FRAMEdocsCheckRules1.DStable
            Width = 97
          end
        end
      end
    end
    object PGCdetails: TsPageControl
      Left = 0
      Top = 120
      Width = 780
      Height = 359
      ActivePage = TSHcriteria
      Align = alClient
      TabOrder = 1
      SkinData.SkinSection = 'PAGECONTROL'
      object TSHcriteria: TsTabSheet
        Caption = 'Criteria'
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        inline FRAMEdocsCheckRulesCriteria1: TFRAMEdocsCheckRulesCriteria
          Left = 0
          Top = 0
          Width = 772
          Height = 331
          Align = alClient
          ParentBackground = False
          TabOrder = 0
          inherited cxDBG: TcxGrid
            Width = 772
            Height = 297
          end
          inherited PNLfooter: TsPanel
            Top = 297
            Width = 772
          end
        end
      end
      object TSHdocuments: TsTabSheet
        Caption = 'Documents'
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        inline FRAMEdocsCheckRulesDocs1: TFRAMEdocsCheckRulesDocs
          Left = 0
          Top = 0
          Width = 772
          Height = 331
          Align = alClient
          ParentBackground = False
          TabOrder = 0
          inherited cxDBG: TcxGrid
            Width = 772
            Height = 297
          end
```
<!-- tabs:end -->

