<!-- tabs:start -->

#### **Documentation**

# Documentation for `MebEntityLink` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `MebEntityLink` code unit is designed to manage and edit entity links in an e-business application. It provides a user interface for viewing, modifying, and saving entity link data. The main objective is to allow users to interact with entity link records, including their original and modified states, ensuring data integrity and proper handling of changes.

### Technologies Used:
- **Delphi**: The code is written in Delphi, utilizing its VCL (Visual Component Library) for UI components.
- **Third-party Libraries**: Includes components like `TsPanel`, `TFRAMEebEntityLink`, and `TFRAMEebEntityLinkOldValue` for UI and data handling.
- **Service Layer**: Utilizes `EbEntityLinkServiceUtils` for backend service interactions.

### Form Type:
This is a **form** with the following elements:
- **Form Elements**:
  - `FRAMEebEntityLink1`: Displays the current entity link data.
  - `FRAMEebEntityLinkOldValue1`: Displays the original entity link data for comparison.
  - `ICBOentity_tp`, `ICBOeb_entity_type`, `ICBOeb_Party_Tp`: Combo boxes for selecting entity types and parties.
- **Form Actions**:
  - **Save**: Saves the modified entity link data.
  - **Cancel**: Cancels the operation and reverts changes.

---

## 2. Functionality Description:

### User/Software Actions:
- View current and original entity link data.
- Modify entity link details using combo boxes.
- Save or cancel changes.

### Main Components:
- **`FRAMEebEntityLink1`**: Displays the current entity link data.
- **`FRAMEebEntityLinkOldValue1`**: Displays the original entity link data.
- **Combo Boxes**: Allow users to select and modify entity types and parties.

### Pseudo-code for Actions and Events:
- **On Form Creation**:
  - `if form created then initialize components and load data`.
- **On Save Button Click**:
  - `if save button clicked then validate data and save changes`.
- **On Cancel Button Click**:
  - `if cancel button clicked then revert changes and close form`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is created using `m_CreateFormEdit`.
   - UI components are initialized, and data is loaded using `m_getData`.
2. **User Interaction**:
   - Users can view and modify entity link data.
   - Changes are saved or canceled based on user actions.
3. **Data Handling**:
   - Data is fetched and updated via `EbEntityLinkServiceUtils`.

### Required User Data:
- Entity Type
- Party Type
- Party
- Entity Type
- Entity

---

## 4. Business Rules:

### Actions and Preconditions:
- **Save**: Enabled only if all required fields are filled.
- **Cancel**: Always enabled.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- "Required field not completed" if a required field is empty.
- "Invalid data" if the entered data does not meet validation criteria.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- **Entity Type**: Must be selected from the combo box.
- **Party Type**: Must be selected from the combo box.
- **Party**: Must be selected from the combo box.

---

## 5. Main Functions:

### `m_CreateFormEdit`:
- **Purpose**: Creates and initializes the form.
- **Logic**: Instantiates the form and sets up its components.

### `m_getData`:
- **Purpose**: Loads data into the form.
- **Logic**: Fetches data from the service layer and populates the UI components.

---

## 6. API Service Consumption:

### Service Name: `EbEntityLinkServiceUtils`
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: `{ "ebEntityType": "string", "ebPartyTp": "string", "ebParty": "string", "entityTp": "string", "entity": "string" }`.
- **Data Received**: `{ "status": "success", "data": "EntityLink object" }`.
- **Purpose**: Fetch and update entity link data.
- **Error Handling**: Displays error messages if the service call fails.

---

## 7. Conditional Fields (Form Logic):

- **"Entity Type" Combo Box**: Visible and editable only when the form is in edit mode.

---

## 8. Dependencies:

### External Libraries:
- **`TsPanel`**: Used for panel components.
- **`TFRAMEebEntityLink`**: Custom frame for displaying entity link data.
- **`TFRAMEebEntityLinkOldValue`**: Custom frame for displaying original entity link data.

### Custom Components:
- **`TFRAMEebEntityLink`**: Displays current entity link data.
- **`TFRAMEebEntityLinkOldValue`**: Displays original entity link data.

---

## 9. Fields and Validations Listing:

### Fields:
- **Entity Type**:
  - Type: Combo Box
  - Required: Yes
- **Party Type**:
  - Type: Combo Box
  - Required: Yes
- **Party**:
  - Type: Combo Box
  - Required: Yes

### Mapping:
- **Entity Type**: Maps to `EntityTp` in the database.
- **Party Type**: Maps to `ebPartyTp` in the database.
- **Party**: Maps to `ebParty` in the database.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Form Initialization] --> [Load Data] --> [User Interaction]
    --> [Save Changes] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Form: Open Form
Form --> Service: Fetch Data
Service --> Form: Return Data
User --> Form: Modify Data
Form --> Service: Save Data
Service --> Form: Confirm Save
```

### Code Snippets:
```delphi
// Create and initialize the form
Result := TFORMMebEntityLink.Create(AOwner);

// Load data into the form
procedure TFORMMebEntityLink.m_getData;
begin
  // Fetch and populate data
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- **Optimization of Resources**:
  - `lv_MasterFrame := TFRAMEBaseEditSOA(kneUtils.TkneGeneric.fg_GetMasterFrame(Self));`
- **Service Parameters**:
  - `lv_MasterFrame.ServiceParams.ShowInactives := True;`

---

## 12. Conclusion:

The `MebEntityLink` code unit provides a robust solution for managing entity links in an e-business application. It ensures data integrity by allowing users to view and modify both current and original entity link data. However, the code lacks explicit error handling and default field values, which could be improved.

---

## 13. Short Summary:

The `MebEntityLink` form facilitates the management of e-business entity links, allowing users to view, modify, and save entity link data while maintaining data integrity through service interactions.#### **MebEntityLink.pas**

```
unit MebEntityLink;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBEdit, ActnList, ImgList, knePrivileges, StdCtrls, Buttons,
  sBitBtn, sSpeedButton, ToolWin, ComCtrls, acCoolBar, ExtCtrls, sPanel,
  kneEnterAsTab, kneFREditSOA, kneFRCtrlEditSOA, FRebEntityLink,
  FRebEntityLinkOldValue;

type
  TFORMMebEntityLink = class(TFORMkneBaseEdit)
    PNLbackground: TsPanel;
    FRAMEebEntityLink1: TFRAMEebEntityLink;
    FRAMEebEntityLinkOldValue1: TFRAMEebEntityLinkOldValue;
  private
    { Private declarations }
  public
    { Public declarations }

    class function m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit; override;
    procedure m_getData; override;
  end;

var
  FORMMebEntityLink: TFORMMebEntityLink;

implementation

{$R *.dfm}

uses
  kneUtils,
  //--- ServiceUtils
  EbEntityLinkServiceUtils;

class function TFORMMebEntityLink.m_CreateFormEdit(
  const AOwner: TComponent): TFORMkneBaseEdit;
begin
	// Substituir pelo nome do form
  Result := TFORMMebEntityLink.Create(AOwner);
end;




procedure TFORMMebEntityLink.m_getData;
var
  lv_MasterFrame: TFRAMEBaseEditSOA;
  lv_params : TStringList;
begin

  Screen.Cursor := crHourGlass;
  // optimiza��o de recursos
  lv_MasterFrame := TFRAMEBaseEditSOA(kneUtils.TkneGeneric.fg_GetMasterFrame(Self));

  //Neste Form � utilizado para permitir guardar o registo original, em MODIFY,
  // para que o servi�o possa apagar o reg.Original e depois gravar o registo alterado


  FRAMEebEntityLinkOldValue1.MasterSource := lv_MasterFrame.DStable;

  // parametros standard de servi�os
  lv_MasterFrame.ServiceParams.ShowInactives := True;
  lv_params := nil;
  try
    lv_params := TStringList.Create();
    TkneGeneric.SplitString(mv_KeyValues, lv_params, ';');

    with TEbEntityLinkServiceUtils(lv_MasterFrame.ProviderService).Params do
    begin
      if lv_params.Count > 4 then
      begin
        ebEntityType := lv_params.Strings[0];
        ebPartyTp := lv_params.Strings[1];
        ebParty := lv_params.Strings[2];
        entityTp := lv_params.Strings[3];
        entity := lv_params.Strings[4];
      end else
      begin
        ebEntityType := '';
        ebPartyTp := '';
        ebParty := '';
        entityTp  := '';
        entity := '';
      end;
    end;

  finally
    if Assigned(lv_params) then FreeAndNil(lv_params);
  end;

  inherited m_getData;

end;


end.
```

#### **MebEntityLink.dfm**

```
inherited FORMMebEntityLink: TFORMMebEntityLink
  Left = 519
  Top = 336
  Width = 652
  Height = 315
  Caption = 'EBusiness Entity Link'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited PNLtoolbar: TsPanel
    Width = 644
    inherited CLBactions: TsCoolBar
      Width = 644
      Bands = <
        item
          Control = PNbotoes
          ImageIndex = -1
          MinHeight = 41
          Width = 640
        end>
      inherited PNbotoes: TsPanel
        Width = 627
      end
    end
  end
  object PNLbackground: TsPanel [2]
    Left = 0
    Top = 41
    Width = 644
    Height = 247
    Align = alClient
    TabOrder = 1
    SkinData.SkinSection = 'PANEL'
    inline FRAMEebEntityLinkOldValue1: TFRAMEebEntityLinkOldValue
      Left = 1
      Top = 1
      Width = 642
      Height = 245
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 1
      inherited PNLfooter: TsPanel
        Top = 211
        Width = 642
      end
    end
    inline FRAMEebEntityLink1: TFRAMEebEntityLink
      Left = 1
      Top = 1
      Width = 642
      Height = 245
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      inherited PNLfooter: TsPanel
        Top = 211
        Width = 642
      end
      inherited PNL2: TsPanel
        Width = 642
        inherited ICBOentity_tp: TcxDBImageComboBox
          DataBinding.DataField = 'EntityTp'
          Properties.OnEditValueChanged = nil
          Width = 117
        end
        inherited ICBOeb_entity_type: TcxDBImageComboBox
          Width = 117
        end
        inherited ICBOeb_Party_Tp: TcxDBImageComboBox
          Width = 205
        end
      end
    end
  end
end
```
<!-- tabs:end -->

