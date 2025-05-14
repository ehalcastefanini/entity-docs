<!-- tabs:start -->

#### **Documentation**

# Documentation for `LebEntityLink` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `LebEntityLink` code unit is designed to manage and display a list of "Entity Links" in a grid format. It provides functionalities such as creating, modifying, viewing, deleting, and searching for entity links. The main objective is to offer a user-friendly interface for managing these links, including advanced search capabilities and selection options.

### Technologies Used:
- **Delphi**: The code is written in Delphi, utilizing its VCL (Visual Component Library) for UI components.
- **Third-party Libraries**: Includes libraries like `cxGrid` for grid display, `sSkinProvider` for UI theming, and `kneCBListSOA` for list management.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **Entity Type** (`ICBOentity_tp`): ComboBox (Image ComboBox).
2. **EB Entity Type** (`ICBOeb_entity_type`): ComboBox (Image ComboBox).
3. **EB Party** (`EDTeb_Party`): Text field.
4. **EB Party Type** (`ICBOeb_Party_Tp`): ComboBox (Image ComboBox).

#### Grid Actions and Their Effects:
1. **Delete Button** (`BTNdelete`): Deletes selected records from the grid.
2. **Search Button**: Refreshes the grid based on search criteria.
3. **Advanced Search**: Allows users to perform detailed searches using multiple criteria.

---

## 2. Functionality Description:

### User/Software Actions:
1. **Delete Records**: Users can delete selected records from the grid.
2. **Search Records**: Users can search for records using basic or advanced criteria.
3. **Create, Modify, View Records**: Users can open forms to create, modify, or view records.

### Main Components:
1. **Grid (`cxDBGlist`)**: Displays the list of entity links.
2. **Search Area (`FRAMEfindCriteriaEbEntityLink1`)**: Provides fields for entering search criteria.
3. **Action Buttons**: Includes buttons for delete, search, and other actions.

### Pseudo-code for Actions and Events:
- **Delete Button Click**:
  ```
  if button clicked then
    if grid is not empty then
      if delete records function returns true then
        refresh grid
  ```
- **Search Button Click**:
  ```
  if search button clicked then
    clear existing criteria
    set criteria from search fields
    refresh grid
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is created using `CreateListForm`.
   - The `Initialize` method sets up the service provider and default parameters.
2. **User Interaction**:
   - Users interact with the grid and action buttons.
   - Clicking the delete button triggers the `BTNdeleteClick` event.
   - Clicking the search button applies the search criteria and refreshes the grid.

### Functions and File Locations:
1. **`CreateListForm`** (File: `LebEntityLink`):
   - Creates the form and initializes it.
2. **`Initialize`** (File: `LebEntityLink`):
   - Sets up the service provider and default parameters.
3. **`BTNdeleteClick`** (File: `LebEntityLink`):
   - Handles the delete button click event.
4. **`SetupParams`** (File: `LebEntityLink`):
   - Configures the search parameters.

### Data Required:
- Search criteria (e.g., entity type, party type).
- Selected records for deletion.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Delete Action**:
   - Preconditions: A record must be selected in the grid.
   - Action: Deletes the selected record(s) and refreshes the grid.
2. **Search Action**:
   - Preconditions: Search criteria must be entered.
   - Action: Filters the grid based on the criteria.

### Available Filters:
- **Entity Type**.
- **EB Entity Type**.
- **EB Party**.
- **EB Party Type**.

### Error Messages:
- "No records selected" if no record is selected for deletion.
- "Search criteria not provided" if search is attempted without criteria.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- **Entity Type**: Must be a valid selection from the combo box.
- **EB Party**: Must be a valid text input.

---

## 5. Main Functions:

1. **`CreateListForm`**:
   - Creates and initializes the form.
2. **`Initialize`**:
   - Sets up the service provider and default parameters.
3. **`SetupParams`**:
   - Configures the search parameters.
4. **`BTNdeleteClick`**:
   - Handles the delete button click event.

---

## 6. API Service Consumption:

### Service Name: `EbEntityLinkServiceUtils`
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Search criteria and selected records.
- **Data Received**: List of entity links.
- **Purpose**: Fetch, update, or delete entity links.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **Search Criteria Fields**:
  - Visible only when the search area is expanded.

---

## 8. Dependencies:

### External Libraries:
1. **`cxGrid`**: Used for grid display.
2. **`sSkinProvider`**: Provides UI theming.
3. **`kneCBListSOA`**: Manages list operations.

### Custom Components:
1. **`FRAMEfindCriteriaEbEntityLink`**: Custom frame for search criteria.

---

## 9. Fields and Validations Listing:

1. **Entity Type** (`ICBOentity_tp`):
   - Type: ComboBox.
   - Validation: Must be a valid selection.
2. **EB Entity Type** (`ICBOeb_entity_type`):
   - Type: ComboBox.
   - Validation: Must be a valid selection.
3. **EB Party** (`EDTeb_Party`):
   - Type: Text.
   - Validation: Must be a valid text input.
4. **EB Party Type** (`ICBOeb_Party_Tp`):
   - Type: ComboBox.
   - Validation: Must be a valid selection.

---

## 10. Examples and Diagrams:

### Flowchart:
(Not applicable as the code does not provide sufficient details for a flowchart.)

### Sequence Diagram:
(Not applicable as the code does not provide sufficient details for a sequence diagram.)

### Code Snippets:
```delphi
procedure TFORMLebEntityLink.BTNdeleteClick(Sender: TObject);
begin
  inherited;
  if not CDSlist.IsEmpty then
  begin
    if m_DeleteRecords then
      Search; // Refresh grid
  end;
end;
```

### Screenshots:
HTML representation of the grid:
```html
<table style="width:100%; border:1px solid black;">
  <thead>
    <tr>
      <th>Entity Type</th>
      <th>EB Entity Type</th>
      <th>EB Party</th>
      <th>EB Party Type</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Type 1</td>
      <td>Type A</td>
      <td>Party 1</td>
      <td>Party Type X</td>
    </tr>
    <tr>
      <td>Type 2</td>
      <td>Type B</td>
      <td>Party 2</td>
      <td>Party Type Y</td>
    </tr>
  </tbody>
</table>
```

---

## 11. Important Comments in the Code:

- **`CreateListForm`**: Initializes the form and sets up the service provider.
- **`BTNdeleteClick`**: Handles the delete button click event.

---

## 12. Conclusion:

The `LebEntityLink` code unit provides a robust interface for managing entity links. Its strengths include a well-structured grid display and advanced search capabilities. However, error handling and API endpoint details are not explicitly defined, which could limit its usability in certain scenarios.

---

## 13. Short Summary:

The `LebEntityLink` code unit manages entity links through a grid interface, offering functionalities like search, delete, and advanced filtering. It integrates with a service provider for data operations and supports user-friendly interactions.#### **LebEntityLink.pas**

```
unit LebEntityLink;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBListSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, knePrivileges, DBClient,
  cxEditRepositoryItems, ImgList, ActnList, sSkinProvider, ExtCtrls,
  sBevel, StdCtrls, sLabel, sScrollBox, cxGridLevel, cxClasses, cxControls,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, kneEnterAsTab, kneFRGridManager, Buttons,
  sSpeedButton, ToolWin, ComCtrls, acCoolBar, sBitBtn, sPanel, sSplitter,
  kneCBList, kneFRfindCriteria, FRfindCriteriaEbEntityLink;

type
  TFORMLebEntityLink = class(TFORMkneCBListSOA)
    FRAMEfindCriteriaEbEntityLink1: TFRAMEfindCriteriaEbEntityLink;
    BTNdelete: TsBitBtn;
    ACLeditingActions_deriv: TActionList;
    ACTnew_deriv: TAction;
    ACTmodify_deriv: TAction;
    ACTview_deriv: TAction;
    ACTsearchArea_deriv: TAction;
    ACTadvancedSearch_deriv: TAction;
    ACTdelete: TAction;
    procedure BTNdeleteClick(Sender: TObject);
    procedure ACTdeleteExecute(Sender: TObject);
  private
    function m_DeleteRecords: Boolean;

    { Private declarations }
  protected
    function SetupParams: Boolean; override;
  public
    { Public declarations }
    class function CreateListForm(
      const AOwner: TComponent): TFORMkneCBList;                   virtual;
    class procedure Initialize(const pv_FormList: TFORMkneCBList); override;
    procedure CreateEditor;                                        override;
  end;

var
  FORMLebEntityLink: TFORMLebEntityLink;

implementation

{$R *.dfm}
uses
    kneUtils
    , EbEntityLinkServiceUtils
    , MebEntityLink;

class function TFORMLebEntityLink.CreateListForm(
  const AOwner: TComponent): TFORMkneCBList;
begin

  Result := TFORMLebEntityLink.Create(AOwner);

  Initialize(Result);
end;

class procedure TFORMLebEntityLink.Initialize(
  const pv_FormList: TFORMkneCBList);
begin
  inherited;

  with TFORMkneCBListSOA(pv_FormList) do
  begin
  	ProviderService := TEbEntityLinkServiceUtils.Create(pv_FormList);
  	AutoLoad        := False;
  	ServiceParams.ShowInactives := True;
    ShowSelectionArea := True;
    SelectionField := 'selected';
    SelectionFieldCheckedValue := 'True';
    SelectionFieldUncheckedValue := 'False';
  end;
end;

procedure TFORMLebEntityLink.CreateEditor;
begin
  inherited;
  EditorForm := TFORMMebEntityLink.Create(Self);
end;

function TFORMLebEntityLink.SetupParams: Boolean;
begin
  ServiceParams.ClearCriteria;
  ServiceParams.Criteria := FRAMEfindCriteriaEbEntityLink1.GetCriteriaValues;

  Result := True;
end;

procedure TFORMLebEntityLink.BTNdeleteClick(Sender: TObject);
begin
  inherited;
  if not CDSlist.IsEmpty then
  begin
    if m_DeleteRecords then
      Search;   // refrescar a grelha
```

#### **LebEntityLink.dfm**

```
inherited FORMLebEntityLink: TFORMLebEntityLink
  Left = 440
  Top = 134
  Caption = 'EBusiness Entity Link List'
  PixelsPerInch = 96
  TextHeight = 13
  inherited SPLlist: TsSplitter
    Top = 168
  end
  inherited PNLsearchArea: TsPanel
    Height = 124
    inherited PNLsearchButtons: TsPanel
      Height = 122
      TabOrder = 0
    end
    inherited SRBcriteria: TsScrollBox
      Height = 122
      TabOrder = 1
      inherited PNLcriteria: TsPanel
        Height = 118
        inline FRAMEfindCriteriaEbEntityLink1: TFRAMEfindCriteriaEbEntityLink
          Left = 1
          Top = 1
          Width = 689
          Height = 116
          Align = alClient
          ParentBackground = False
          TabOrder = 0
          inherited ICBOentity_tp: TcxImageComboBox
            Properties.OnCloseUp = nil
            Properties.OnEditValueChanged = nil
            Width = 121
          end
          inherited ICBOeb_entity_type: TcxImageComboBox
            Width = 121
          end
          inherited EDTeb_Party: TcxTextEdit
            Width = 489
          end
          inherited ICBOeb_Party_Tp: TcxImageComboBox
            Width = 277
          end
        end
      end
    end
  end
  inherited PNLactions: TsPanel
    inherited CLBlistActions: TsCoolBar
      inherited PNLstandardActions: TsPanel
        inherited BTNseparator1: TsSpeedButton
          Left = 313
        end
        inherited BTNseparator2: TsSpeedButton
          Left = 476
        end
        inherited BTNseparator: TsSpeedButton
          Left = 576
        end
        inherited PNLgridManager: TsPanel
          Left = 484
        end
        inherited PNLrecordActions: TsPanel
          Width = 312
          object BTNdelete: TsBitBtn
            Left = 228
            Top = 2
            Width = 75
            Height = 30
            Action = ACTdelete
            Caption = 'Delete'
            TabOrder = 3
            SkinData.SkinSection = 'SPEEDBUTTON'
            ImageIndex = 7
            Images = IMLbuttons
          end
        end
        inherited PNLsearchActions: TsPanel
          Left = 321
        end
      end
    end
  end
  inherited PNLlist: TsPanel
    Top = 174
    Height = 377
    inherited SPT1: TsSplitter
      Left = 783
      Height = 377
      Enabled = False
    end
    inherited PNLlistArea: TsPanel
      Width = 783
      Height = 377
      inherited cxDBGlist: TcxGrid
        Width = 781
        Height = 351
      end
      inherited PNLselectionArea: TsPanel
        Width = 781
      end
```
<!-- tabs:end -->

