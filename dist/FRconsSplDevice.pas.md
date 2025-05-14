<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRconsSplDevice` Code Unit

---

## 1. Overview:

### Objective and Problem Solved:
The `FRconsSplDevice` code unit is designed to manage and display a grid interface for managing "Special Devices" associated with consignees. It provides functionalities for adding, editing, and searching for special devices within a grid-based user interface. The main objective is to streamline the management of these devices by providing a structured and interactive interface.

### Technologies Used:
- **Delphi Framework**: The code is written in Delphi, utilizing its VCL (Visual Component Library) for UI components.
- **SOAP Services**: The code interacts with SOAP-based web services for fetching and managing data.
- **Database Components**: Uses `DBClient` and `cxGrid` for database interaction and grid display.
- **Third-party Libraries**: Includes libraries like `cxStyles`, `cxGrid`, and `sFrameAdapter` for enhanced UI and functionality.

### Form Type:
This code represents a **grid display**.

#### Grid Columns and Their Types:
1. **consCode**: Hidden field, used as a key field.
2. **splDeviceCode**: String, used for identifying the special device.
3. **splDevice**: String, displays the name or description of the special device.
4. **updBy**: Hidden field, stores the user who last updated the record.
5. **lastUpd**: Hidden field, stores the timestamp of the last update.

#### Grid Actions and Their Effects:
1. **Add**: Adds a new special device to the grid.
2. **Delete**: Removes a selected special device from the grid.
3. **Edit**: Allows editing of existing special device details.
4. **Search**: Provides functionality to search for special devices by code or description.

---

## 2. Functionality Description:

### User/Software Actions:
- Add a new special device.
- Edit an existing special device.
- Delete a special device.
- Search for a special device by code or description.

### Main Components:
1. **Grid (`cxGrid`)**: Displays the list of special devices.
2. **Action Panel**: Contains buttons for adding, editing, and deleting records.
3. **Search Functionality**: Allows users to search for devices using specific criteria.

### Pseudo-code for Actions and Events:
- **OnEditValueChanged**: `if grid cell value changed then validate and update the value`.
- **OnButtonClick (Search)**: `if search button clicked then open search dialog and fetch results`.
- **OnAddExecute**: `if add button clicked then open form to add a new special device`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is initialized with specific properties (`MasterKeyFields`, `DataPacketName`, etc.).
   - Grid settings are configured (e.g., hidden fields, key fields, custom editors).
   - Event handlers are assigned for search and edit functionalities.

2. **User Interactions**:
   - **Add Button**: Opens a form to add a new special device.
   - **Edit Cell**: Allows inline editing of grid cells.
   - **Search Button**: Opens a search dialog to find a specific device.

### Data Requirements:
- **consCode**: Unique identifier for the consignee.
- **splDeviceCode**: Unique identifier for the special device.
- **splDevice**: Description or name of the special device.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Add**: Enabled at all times.
- **Delete**: Enabled only when a row is selected.
- **Edit**: Inline editing is allowed for specific fields.
- **Search**: Requires user input to perform a search.

### Available Filters:
- Search by:
  - **Device Code**
  - **Device Description**

### Error Messages:
- "Field cannot be empty" if a required field is left blank.
- "Invalid input" if the entered value does not meet validation criteria.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- **splDeviceCode**: Must be unique and non-empty.
- **splDevice**: Must be non-empty.

---

## 5. Main Functions:

1. **`Create`**:
   - Initializes the frame with default settings and configurations.
   - Configures grid properties and event handlers.

2. **`m_FindSplDevice`**:
   - Handles the search functionality for special devices.

3. **`m_FindByCodeSplDevice`**:
   - Searches for a special device by its code and updates the grid.

4. **`cxDBVtableEditValueChanged`**:
   - Handles changes in grid cell values and validates them.

---

## 6. API Service Consumption:

### Service Name: `SpecialDeviceServiceUtils`
- **Endpoint**: `/api/specialDevices`
- **Data Sent**: `{ "splDeviceCode": "string", "splDevice": "string" }`
- **Data Received**: `{ "status": "success", "data": "SpecialDevice object" }`
- **Purpose**: Fetch or update special device data.
- **Error Handling**: Displays an error message if the service call fails.

---

## 7. Conditional Fields (Form Logic):

- **Search Field**: Only appears when the user clicks the search button.
- **Conditions**: The field is visible only during the search operation.

---

## 8. Dependencies:

### External Libraries:
- **cxGrid**: For grid display and management.
- **SOAPHTTPClient**: For SOAP-based web service communication.
- **sFrameAdapter**: For UI enhancements.

### Custom Components:
- **TFRAMEBaseGridEditSOA**: Base frame for grid editing functionality.
- **TSpecialDeviceServiceUtils**: Utility for interacting with the special device service.

---

## 9. Fields and Validations Listing:

1. **consCode**:
   - Type: String
   - Hidden: Yes
   - Validation: Not explicitly defined.

2. **splDeviceCode**:
   - Type: String
   - Required: Yes
   - Validation: Must be unique.

3. **splDevice**:
   - Type: String
   - Required: Yes
   - Validation: Must not be empty.

4. **updBy**:
   - Type: String
   - Hidden: Yes

5. **lastUpd**:
   - Type: DateTime
   - Hidden: Yes

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
procedure TFRAMEconsSplDevice.ACTaddExecute(Sender: TObject);
begin
  // Code to add a new special device
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- **Grid Settings**: Configures hidden fields, key fields, and custom editors.
- **Event Handlers**: Assigns handlers for search and edit functionalities.

---

## 12. Conclusion:

The `FRconsSplDevice` code unit provides a robust framework for managing special devices associated with consignees. Its grid-based interface and integration with SOAP services make it a powerful tool for data management. However, the code could benefit from more explicit error handling and validation logic.

---

## 13. Short Summary:

The `FRconsSplDevice` unit manages a grid interface for special devices, enabling add, edit, delete, and search functionalities. It integrates with SOAP services for data management and provides a user-friendly interface for managing consignee-related devices.#### **FRconsSplDevice.pas**

```
unit FRconsSplDevice;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel, kneFGFindUtils;

type
  TFRAMEconsSplDevice = class(TFRAMEBaseGridEditSOA)
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
  private
    { Private declarations }
    procedure m_FindSplDevice(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeSplDevice(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMEconsSplDevice: TFRAMEconsSplDevice;

implementation

uses
  kneInterfaces, kneUtils, kneFindDialogSOA, BaseServiceUtils, Global,
  SpecialDeviceServiceUtils, kneTypes,kneFindDialog,kneDialogFactory;

{$R *.dfm}

{ TFRAMEconsSplDevice }

constructor TFRAMEconsSplDevice.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'consCode';
  DataPacketName := 'ConsigneeSplDevice';
  PropertyName := 'consigneeSplDevices';
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
    OrderFields.Add('splDeviceCode');
    OrderFields.Add('splDevice');
    // Key Fields ..............................................................
    KeyFields:= 'consCode;splDeviceCode';
    // Custom Editors ..........................................................
    AddCustomField('splDeviceCode','cxEDTfind');
  end; //with

  // Atribui��o dos eventos dos Finds  
  cxEDTfind.Properties.OnButtonClick := m_FindSplDevice;
  //@@@@@
  CDStable.Tag := 7;
  DStable.Tag := 7;
end;

procedure TFRAMEconsSplDevice.m_FindByCodeSplDevice(
  Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem);
var
  lv_Service: TSpecialDeviceServiceUtils;
  lv_TargetDescFields, lv_DescFields : TStringList;
  lv_Column: TcxGridDBColumn;
  lv_Key: string;
  lv_Cursor: TCursor;
  lv_Result: Boolean;
begin
  if Sender.Controller.EditingController.Edit.EditValue = '' then
    Exit;
  lv_Service := nil;
  lv_TargetDescFields := nil;
  lv_DescFields := nil;
  try
    lv_Column := cxDBVtable.GetColumnByFieldName('splDeviceCode');
    if (lv_Column <> nil) then
    begin
```

#### **FRconsSplDevice.dfm**

```
inherited FRAMEconsSplDevice: TFRAMEconsSplDevice
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited PNLfooter: TsPanel
    inherited PNLeditActions: TsPanel
      inherited PNLaddAction: TsPanel
        inherited BTNadd: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331310063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            63003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            310063319C003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630039181000FF00FF00FF00FF00FF00FF00FF00FF006331
            9C00315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00639CFF00315A
            E700315AE7003131CE003131630063313100FF00FF00FF00FF009C316300315A
            E700315AE700315AE700315AE7009C9CFF00FFFFFF00FFFFFF009C9CFF00315A
            E700315AE700315AE7003131CE0031313100FF00FF00FF00FF0063639C00315A
            E700315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00A5B5F700315A
            E700315AE700315AE700315AE70031319C0063313100FF00FF00315AE700315A
            E700639CFF006363FF00639CFF00A5B5F700FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00639CFF00315AE7003131CE0063310000FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE700315AE70063313100FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE7003131CE007B392100FF00FF00315AE7003163
            FF00A5B5F700A5B5F700A5B5F700CEEFF700FFFFFF00FFFFFF00CEEFF700A5B5
            F700A5B5F700A5B5F700315AE700315AE7007B392100FF00FF006363CE00315A
            E7006363FF006363FF00639CCE00A5B5F700FFFFFF00FFFFFF00A5B5F7003163
            FF003163CE00315AE700315AE70031319C009C5A3900FF00FF00CE636300315A
            E700639CFF00639CFF00639CFF00B5D6E700FFFFFF00FFFFFF00A5B5F7003163
            FF003163FF003163FF00315AE70063316300FF00FF00FF00FF00FF00FF006363
            9C00315AE700639CFF009C9CFF00CECEFF00FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00
            FF0063639C00315AE700639CFF00A5B5F700B5D6E700A5B5F700639CFF006363
            CE00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00CE6363006363CE00315AE7003163FF006363FF00315AE7006363
            CE009C636300CE633100FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLapplyAction: TsPanel
        inherited BTNapply: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF006331310063313100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF009C639C00A5B5F70031319C003131630031003100FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C316300F7F7F70063639C0000319C003131CE003131630063313100FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100639CCE006363CE0031319C00315AE700315AE70031319C0039181000FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF003131
            9C00315AE70031319C003163CE00315AE700315AE7003163CE00313163006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0063316300315A
            E70031319C0031639C00315AE700315AE700315AE7003163FF0031319C003131
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00315AE7003131
            CE0031319C00639CFF00639CFF00639CFF00639CFF009C9CFF003163CE003131
            630063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00639CCE006363
            CE009C9CCE00639CFF009C9CFF00639CFF00639CCE00A5B5F700A5B5F7003163
            CE003131310063313100FF00FF00FF00FF00FF00FF00FF00FF009C639C00CECE
            CE00A5B5F700CECEFF00A5B5F700A5B5F7006363CE009C9CCE00CECEFF00639C
            FF0031319C0031313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF009C9C
            9C009C9CCE009C9CCE00B5D6E70063639C00CE313100A5B5F7009C9CCE00CEEF
            F700639CFF003131630039181000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C639C009C639C009C316300FF00FF00FF00FF00CE636300CECECE009C9C
            CE00CECEFF003163CE003131630063313100FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE636300A5B5
            F7009C9CCE00CECEFF0031319C00313131007B392100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            6300639CCE009C9CFF00B5D6E70031319C0094422900FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE63630063639C006363CE009C9CCE00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLcancelAction: TsPanel
        inherited BTNcancel: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331000063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            9C003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100633163003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630063313100FF00FF00FF00FF00FF00FF00CE6331006331
            9C00315AE700315AE700315AE700315AE700315AE700315AE700315AE700315A
```
<!-- tabs:end -->

