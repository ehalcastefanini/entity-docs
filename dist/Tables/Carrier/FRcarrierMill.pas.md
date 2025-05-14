<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcarrierMill` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRcarrierMill` code unit defines a Delphi frame (`TFRAMEcarrierMill`) that extends a base grid-editing frame (`TFRAMEBaseGridEditSOA`). Its primary purpose is to manage and display a grid of carrier and mill data, allowing users to interact with the data through actions such as adding, deleting, and editing records. It also includes functionality for validating and managing SAP codes associated with the mills.

This frame is designed to be part of a larger system, likely a logistics or supply chain management application, where users need to manage relationships between carriers and mills efficiently.

### Technologies Used:
- **Delphi VCL Framework**: For building the user interface and handling events.
- **SOAP Services**: For interacting with external services (e.g., SAP code validation).
- **Database Components**: For managing and displaying data in a grid format.
- **Custom Components**: Includes custom grid management and utility components.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **mill**: Read-only field.
2. **millCarrier**: Read-only field.
3. **stat**: Read-only field.
4. **millDtime**: Read-only field.
5. **sapCode**: Read-only field.

#### Grid Actions and Their Effects:
1. **Add**: Adds a new record to the grid.
2. **Delete**: Deletes the selected record from the grid.
3. **Double-click on a cell**: Triggers an event to handle cell-specific actions.
4. **Edit value change**: Updates the grid when a cell value is modified.

---

## 2. Functionality Description:

### User/Software Actions:
1. **Add a Record**: Users can add a new carrier-mill relationship.
2. **Delete a Record**: Users can delete an existing relationship.
3. **Edit a Record**: Users can modify specific fields in the grid.
4. **Validate SAP Codes**: The system validates SAP codes for mills.
5. **Search Mills**: Users can search for mills using specific criteria.

### Main Components:
1. **Grid (`cxGrid`)**: Displays the carrier-mill data.
2. **Actions (`ACTadd`, `ACTdelete`)**: Handle user actions like adding or deleting records.
3. **SAP Code Management**: Validates and manages SAP codes for mills.
4. **Event Handlers**: Handle user interactions with the grid.

### Pseudo-code for Actions and Events:
- **OnCellDblClick**: `if cell double-clicked then execute cell-specific action`.
- **OnEditValueChanged**: `if cell value changed then update grid`.
- **OnAddAction**: `if add button clicked then open form to add new record`.
- **OnDeleteAction**: `if delete button clicked then remove selected record`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is initialized with specific settings for the grid, including hidden fields, read-only fields, and key fields.
   - Available actions (`ADD`, `DELETE`) are configured.

2. **User Interactions**:
   - Users interact with the grid by double-clicking cells, editing values, or using the add/delete actions.
   - Event handlers respond to these interactions and execute the corresponding logic.

3. **Functions and File Locations**:
   - `CDStableAfterScroll`: Updates the UI when the dataset scrolls.
   - `cxDBVtableCellDblClick`: Handles double-click actions on grid cells.
   - `ACTaddExecute`: Adds a new record.
   - `m_validate`: Validates the current state of the frame.
   - `m_CheckNewSapCode`: Validates a new SAP code.

### Required Data:
- Users must provide valid carrier and mill data.
- SAP codes must be validated before saving.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add Action**:
   - Preconditions: None.
   - Effect: Opens a form to add a new record.
2. **Delete Action**:
   - Preconditions: A record must be selected.
   - Effect: Deletes the selected record.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- "Invalid SAP Code" if the SAP code validation fails.
- "No record selected" if the delete action is triggered without a selected record.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- SAP codes are validated using the `m_CheckNewSapCode` function.

---

## 5. Main Functions:

1. **`m_validate`**: Validates the current state of the frame.
2. **`m_CheckNewSapCode`**: Validates a new SAP code.
3. **`m_SaveSapCodes`**: Saves the SAP codes to the database.
4. **`m_GetSelectedMills`**: Retrieves the selected mills from the grid.

---

## 6. API Service Consumption:

- **Service Name**: SAP Code Validation Service.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: `{ "sapCode": "string" }`.
- **Data Received**: `{ "status": "success", "valid": true/false }`.
- **Purpose**: Validate SAP codes for mills.
- **Error Handling**: Displays an error message if the validation fails.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
1. **SOAPHTTPClient**: For SOAP service interactions.
2. **cxGrid**: For grid display and management.

### Custom Components:
1. **kneFRGridEditSOA**: Base class for grid-editing frames.
2. **kneUtils**: Utility functions.

---

## 9. Fields and Validations Listing:

1. **mill**: (type: string, read-only).
2. **millCarrier**: (type: string, read-only).
3. **stat**: (type: string, read-only).
4. **millDtime**: (type: datetime, read-only).
5. **sapCode**: (type: string, read-only).

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
procedure TFRAMEcarrierMill.ACTaddExecute(Sender: TObject);
begin
  // Logic to add a new record
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- `MasterKeyFields := 'code=carrier';`: Defines the key fields for the grid.
- `AvailableActions := 'ADD;DELETE';`: Configures the available actions for the frame.

---

## 12. Conclusion:

The `FRcarrierMill` code unit provides a robust framework for managing carrier-mill relationships. Its strengths include a well-defined grid interface and SAP code validation. However, the lack of detailed error handling and documentation for API endpoints may limit its usability in some scenarios.

---

## 13. Short Summary:

The `FRcarrierMill` unit manages carrier-mill relationships using a grid interface, supporting actions like adding, deleting, and validating SAP codes. It integrates with SOAP services for SAP code validation and is part of a larger logistics system.#### **FRcarrierMill.pas**

```
unit FRcarrierMill;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, kneUtils;

type
  TFRAMEcarrierMill = class(TFRAMEBaseGridEditSOA)
    procedure CDStableAfterScroll(DataSet: TDataSet);
    procedure cxDBVtableCellDblClick(Sender: TcxCustomGridTableView;
      ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton;
      AShift: TShiftState; var AHandled: Boolean);
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure cxDBVtableFocusedItemChanged(Sender: TcxCustomGridTableView;
      APrevFocusedItem, AFocusedItem: TcxCustomGridTableItem);
    procedure cxDBVtableFocusedRecordChanged(
      Sender: TcxCustomGridTableView; APrevFocusedRecord,
      AFocusedRecord: TcxCustomGridRecord;
      ANewItemRecordFocusingChanged: Boolean);
    procedure cxDBVtableStylesGetContentStyle(
      Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
      AItem: TcxCustomGridTableItem; out AStyle: TcxStyle);
    procedure ACTaddExecute(Sender: TObject);
  private
    { Private declarations }
    FSapCodes: TStringList;
    FHasSapCode: TNotifyWithBooleanEvent;
    FOpenSapCodeForm: TNotifyEvent;
    procedure SetHasSapCode(const Value: TNotifyWithBooleanEvent);
    procedure SetOpenSapCodeForm(const Value: TNotifyEvent);
    procedure m_FindByCodeMill(Sender: TcxCustomGridTableView;
        AItem: TcxCustomGridTableItem);
    procedure m_FindMill(Sender: TObject; AButtonIndex: Integer);
    procedure SetKeyEditing(const EditKey: Boolean); override;
    procedure m_ChangeEditableFields;
    procedure m_SaveSapCodes;
    procedure m_GetSelectedMills;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
    property HasSapCode: TNotifyWithBooleanEvent read FHasSapCode write SetHasSapCode;
    property OpenSapCodeForm: TNotifyEvent read FOpenSapCodeForm write SetOpenSapCodeForm;
    function m_validate: Boolean;  override;
    function m_CheckNewSapCode(const pv_NewSapCode: String): Boolean;
    destructor Destroy; override;
  end;

var
  FRAMEcarrierMill: TFRAMEcarrierMill;
  gv_AddedMills: String;

implementation

uses
  kneTypes, kneFREditSOA, kneFindDialog{#NAVOPTECH2022-785}, Global{#NAVOPTECH2022-875}
  ,kneDialogFactory{#NAVOPTECH2022-785}, kneFGFindUtils{#NAVOPTECH2022-785}
  , MillWithoutDuplicServiceUtils{#NAVOPTECH2022-785};

{$R *.dfm}

{ TFRAMEcarrierMill }

constructor TFRAMEcarrierMill.Create(AOwner: TComponent);
begin
	inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'code=carrier';
  DataPacketName := 'LnkCarrier';         // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'links';          // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin

    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');

    // Ordem Campos ............................................................
    DefineReadOnlyFields('mill;millCarrier;stat;millDtime;sapCode');

    // Key Fields ..............................................................
    KeyFields:= 'mill;carrier';

    //NAVOPTECH2022-785 (cmosilva 26-05-2022)
    // Custom Editors ..........................................................
```

#### **FRcarrierMill.dfm**

```
inherited FRAMEcarrierMill: TFRAMEcarrierMill
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnCellDblClick = cxDBVtableCellDblClick
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
end
```
<!-- tabs:end -->

