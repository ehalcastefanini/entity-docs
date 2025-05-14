<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRconsDelDay` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRconsDelDay` code unit defines a frame (`TFRAMEconsDelDay`) that manages a grid-based interface for editing and managing delivery days for consignees. It provides functionality to display, edit, and validate data related to delivery days, such as allowed days, time ranges, and weekdays. The frame is designed to handle user interactions, validate input, and manage data persistence.

### Technologies Used:
- **Delphi VCL Framework**: For creating the user interface and handling events.
- **TcxGrid**: A grid component from DevExpress for displaying and editing tabular data.
- **TClientDataSet**: For managing in-memory datasets.
- **SOAPHTTPClient**: For consuming SOAP-based web services.
- **Custom Components**: Includes `TcxEditRepositoryCheckBoxItem`, `TcxEditRepositoryMaskItem`, and others for specialized editing and validation.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **seqNum**: Integer (Order field).
2. **allowed**: Boolean (Checkbox editor).
3. **weekDayCode**: String (Custom editor for finding weekdays).
4. **weekDay**: String (Display field).
5. **timeBegin**: Time (Masked editor for time input).
6. **timeEnd**: Time (Masked editor for time input).

#### Grid Actions and Their Effects:
1. **Add**: Adds a new record to the grid.
2. **Delete**: Deletes the selected record from the grid.
3. **Edit**: Allows editing of specific fields in the grid.
4. **Validation**: Validates time fields and other inputs.

---

## 2. Functionality Description:

### User/Software Actions:
1. Add a new delivery day record.
2. Edit existing delivery day records.
3. Delete selected records.
4. Validate input fields (e.g., time format, checkbox values).

### Main Components:
- **Grid (`TcxGrid`)**: Displays the delivery day records.
- **ClientDataSet (`TClientDataSet`)**: Manages the data displayed in the grid.
- **Custom Editors**: Provides specialized input validation and formatting.

### Pseudo-Code for Actions and Events:
- `OnEditValueChanged` event of the grid:
  - `if grid cell value changed then update dataset`.
- `OnNewRecord` event of the dataset:
  - `if new record is created then initialize default values`.
- `OnValidate` event of the time field:
  - `if time value is invalid then show error message`.
- `OnButtonClick` event of the weekday finder:
  - `if button clicked then open weekday selection dialog`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is created and initialized with default settings.
   - Grid columns are configured, and hidden fields are defined.
   - Custom editors are assigned to specific fields.
2. **User Interaction**:
   - Users can add, edit, or delete records in the grid.
   - Input validation is triggered when editing specific fields.
3. **Event Handling**:
   - Events like `OnEditValueChanged`, `OnNewRecord`, and `OnValidate` are triggered based on user actions.

### Data Requirements:
- **Required Fields**:
  - `seqNum`, `allowed`, `weekDayCode`, `timeBegin`, `timeEnd`.
- **Optional Fields**:
  - `weekDay`.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add**:
   - Preconditions: None.
   - Action: Adds a new record with default values.
2. **Delete**:
   - Preconditions: A record must be selected.
   - Action: Deletes the selected record.
3. **Edit**:
   - Preconditions: A record must be selected.
   - Action: Allows editing of specific fields.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
1. "Invalid time format" if the time input does not match the expected format.
2. "Required field not completed" if a mandatory field is left empty.

### Default Field Values:
1. `allowed`: Default "N".
2. `timeBegin` and `timeEnd`: No default value specified.

### Field Validation and Conditions:
1. `allowed`: Must be "Y" or "N".
2. `timeBegin` and `timeEnd`: Must follow the `!90:00;1;_` mask format.

---

## 5. Main Functions:

1. **`Create`**:
   - Initializes the frame and configures grid settings.
2. **`SetColumnState`**:
   - Enables or disables editing for specific columns.
3. **`m_FindWeekDays`**:
   - Opens a dialog to select weekdays.
4. **`cxEDTtimePropertiesValidate`**:
   - Validates the time input format.

---

## 6. API Service Consumption:

- **Service Name**: WeekDayServiceUtils.
- **Purpose**: Fetches weekday data for the `weekDayCode` field.
- **Error Handling**: If the service fails, an error message is displayed.

---

## 7. Conditional Fields (Form Logic):

- **Field**: `weekDayCode`.
- **Condition**: The field is editable only when the `allowed` checkbox is checked.

---

## 8. Dependencies:

### External Libraries:
1. **DevExpress Components**: For grid and editor functionalities.
2. **SOAPHTTPClient**: For consuming SOAP services.

### Custom Components:
1. `TcxEditRepositoryCheckBoxItem`: For checkbox editing.
2. `TcxEditRepositoryMaskItem`: For masked time input.

---

## 9. Fields and Validations Listing:

1. **seqNum**: Integer, required.
2. **allowed**: Boolean, required, values "Y" or "N".
3. **weekDayCode**: String, required.
4. **weekDay**: String, optional.
5. **timeBegin**: Time, required, format `!90:00;1;_`.
6. **timeEnd**: Time, required, format `!90:00;1;_`.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
procedure TFRAMEconsDelDay.cxEDTtimePropertiesValidate(Sender: TObject;
  var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
begin
  if not IsValidTime(DisplayValue) then
  begin
    Error := True;
    ErrorText := 'Invalid time format';
  end;
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

1. **Grid Settings**:
   - Hidden fields: `consCode`, `updBy`, `lastUpd`.
   - Key fields: `consCode`, `seqNum`.
2. **Custom Editors**:
   - `allowed`: Checkbox editor.
   - `timeBegin`, `timeEnd`: Masked time editor.

---

## 12. Conclusion:

The `FRconsDelDay` code unit provides a robust framework for managing delivery day data in a grid interface. It includes features for adding, editing, and validating records. However, the code lacks explicit error handling for API failures and does not define filters for the grid.

---

## 13. Short Summary:

The `FRconsDelDay` unit implements a grid-based interface for managing consignee delivery days, with features for adding, editing, and validating records. It uses DevExpress components and SOAP services for data handling and validation.#### **FRconsDelDay.pas**

```
unit FRconsDelDay;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel;

type
  TFRAMEconsDelDay = class(TFRAMEBaseGridEditSOA)
    cxEDTallowed: TcxEditRepositoryCheckBoxItem;
    cxEDTtime: TcxEditRepositoryMaskItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure CDStableBeforeInsert(DataSet: TDataSet);
    procedure CDStableNewRecord(DataSet: TDataSet);
    procedure cxEDTtimePropertiesValidate(Sender: TObject;
      var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure ACTaddExecute(Sender: TObject);
  private
    { Private declarations }
    mv_FieldNextVal: Integer;

    procedure m_FindWeekDays(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeWeekDays(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure SetColumnState(pv_ColName: String; pv_Edit: Boolean);
  protected
    { Protected declarations }
    procedure SetKeyEditing(const EditKey: Boolean);  override;
  
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMEconsDelDay: TFRAMEconsDelDay;

implementation

uses
  kneInterfaces, kneUtils, kneFindDialogSOA, BaseServiceUtils, Global,
  WeekDayServiceUtils, kneFREditSOA, kneTypes, kneFindDialog, kneDialogFactory,
  kneFGFindUtils;

{$R *.dfm}

{ TFRAMEconsDelDay }

constructor TFRAMEconsDelDay.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'consCode';
  DataPacketName := 'ConsigneeDelDay';
  PropertyName := 'consigneeDelDays';
  FrameType := frtDetail;
  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    // Campos Hidden ...........................................................
    HiddenFields.Add('consCode');
    HiddenFields.Add('updBy');
    HiddenFields.Add('lastUpd');
    // Ordem Campos ............................................................
    OrderFields.Add('seqNum');
    OrderFields.Add('allowed');
    OrderFields.Add('weekDayCode');
    OrderFields.Add('weekDay');
    OrderFields.Add('timeBegin');
    OrderFields.Add('timeEnd');
    // Key Fields ..............................................................
    KeyFields := 'consCode;seqNum';
    // Custom Editors ..........................................................
    AddCustomField('allowed','cxEDTallowed');
    AddCustomField('weekDayCode','cxEDTfind');
    AddCustomField('timeBegin','cxEDTtime');
    AddCustomField('timeEnd','cxEDTtime');
  end; //with

  // Atribui��o dos eventos dos Finds  
  cxEDTfind.Properties.OnButtonClick := m_FindWeekDays;
  //@@@@@
  CDStable.Tag := 5;
```

#### **FRconsDelDay.dfm**

```
inherited FRAMEconsDelDay: TFRAMEconsDelDay
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
      DataController.Summary.FooterSummaryItems = <
        item
        end>
      OptionsView.GroupFooters = gfAlwaysVisible
    end
  end
  inherited CDStable: TClientDataSet
    OnNewRecord = CDStableNewRecord
  end
  inherited cxEDTR: TcxEditRepository
    inherited cxEDTfind: TcxEditRepositoryButtonItem
      Properties.CharCase = ecUpperCase
    end
    object cxEDTallowed: TcxEditRepositoryCheckBoxItem
      Properties.ValueChecked = 'Y'
      Properties.ValueUnchecked = 'N'
    end
    object cxEDTtime: TcxEditRepositoryMaskItem
      Properties.EditMask = '!90:00;1;_'
      Properties.OnValidate = cxEDTtimePropertiesValidate
    end
  end
end
```
<!-- tabs:end -->

