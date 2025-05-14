<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRwarehouseCarrier` Code Unit

---

## 1. Overview:

### Objective and Problem Solved:
The `FRwarehouseCarrier` code unit defines a Delphi frame (`TFRAMEwarehouseCarrier`) that provides a grid-based interface for managing warehouse carriers. It allows users to view, add, and delete carrier details associated with a warehouse. The frame is designed to handle carrier data efficiently, including searching and editing carrier details.

### Technologies Used:
- **Delphi VCL Framework**: For building the user interface and handling events.
- **SOAP Services**: For interacting with external services (e.g., `CarrierServiceUtils`).
- **Database Components**: For managing and displaying data (`DB`, `cxDBData`, `DBClient`).
- **DevExpress Components**: For advanced grid and UI functionalities (`cxGrid`, `cxEditRepositoryItems`).

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **Code**: String (Editable, searchable).
2. **Name**: String (Editable).
3. **Warehouse Code**: String (Hidden).
4. **Updated By**: String (Hidden).
5. **Last Updated**: DateTime (Hidden).

#### Grid Actions and Their Effects:
1. **Add**: Adds a new carrier to the grid.
2. **Delete**: Removes the selected carrier from the grid.
3. **Search**: Allows searching for carriers by code or other criteria.

---

## 2. Functionality Description:

### User/Software Actions:
1. **Add Carrier**: Users can add a new carrier using the "Add" action.
2. **Delete Carrier**: Users can delete a selected carrier using the "Delete" action.
3. **Search Carrier**: Users can search for a carrier by code or other details using the search button.

### Main Components:
- **Grid (`cxDBVtable`)**: Displays carrier details.
- **Search Button (`cxEDTfindCarrierDetail`)**: Allows searching for carriers.
- **Action Panel**: Provides "Add" and "Delete" actions.

### Pseudo-code for Actions and Events:
- `OnClick` event of "Add" button: `if Add button clicked then execute add carrier function`.
- `OnClick` event of "Delete" button: `if Delete button clicked then execute delete carrier function`.
- `OnButtonClick` event of search button: `if search button clicked then execute search carrier function`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is initialized with default settings (e.g., hidden fields, key fields, and available actions).
   - Event handlers are assigned for search and access mode settings.

2. **User Interactions**:
   - Clicking "Add" triggers the `ACTaddExecute` procedure to add a new carrier.
   - Clicking "Delete" removes the selected carrier.
   - Clicking the search button triggers the `m_FindCarrier` procedure to search for a carrier.

### Data Input:
- Users must provide carrier details such as "Code" and "Name" when adding or editing a carrier.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add Carrier**:
   - Preconditions: None.
   - Action: Adds a new carrier to the grid.

2. **Delete Carrier**:
   - Preconditions: A carrier must be selected in the grid.
   - Action: Deletes the selected carrier.

3. **Search Carrier**:
   - Preconditions: None.
   - Action: Searches for a carrier by code or other criteria.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- No error messages are explicitly defined in the code.

### Default Field Values:
- No default values are explicitly defined in the code.

### Field Validation and Conditions:
- **Code**: Must be unique and searchable.
- **Name**: Editable but no specific validation is defined.

---

## 5. Main Functions:

1. **`Create` Constructor**:
   - Initializes the frame with default settings (e.g., hidden fields, key fields, and available actions).

2. **`m_FindCarrier`**:
   - Handles the search functionality for carriers.

3. **`m_SetOnSetAccessMode`**:
   - Configures the grid's column widths and access mode.

4. **`ACTaddExecute`**:
   - Adds a new carrier to the grid.

5. **`cxDBVtableEditValueChanged`**:
   - Handles changes in grid cell values.

---

## 6. API Service Consumption:

- **Service Name**: `CarrierServiceUtils`.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Carrier details (e.g., `Code`, `Name`).
- **Data Received**: Carrier data or confirmation of the operation.
- **Purpose**: To interact with carrier-related data.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
1. **DevExpress Components**: For grid and UI functionalities.
2. **SOAP Services**: For interacting with external services.

### Custom Components:
1. **`TFRAMEBaseGridEditSOA`**: Base class for the frame.
2. **`TCarrierServiceUtils`**: Utility class for carrier-related operations.

---

## 9. Fields and Validations Listing:

1. **Code**:
   - Type: String.
   - Required: Yes.
   - Validation: Must be unique.
   - Constraints: Not explicitly defined in the code.

2. **Name**:
   - Type: String.
   - Required: Yes.
   - Validation: None explicitly defined.

3. **Warehouse Code**:
   - Type: String.
   - Required: No.
   - Hidden: Yes.

4. **Updated By**:
   - Type: String.
   - Required: No.
   - Hidden: Yes.

5. **Last Updated**:
   - Type: DateTime.
   - Required: No.
   - Hidden: Yes.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Frame] --> [Load Grid Data] --> [User Interaction]
    --> [Add Carrier] --> [Save to Database]
    --> [Delete Carrier] --> [Remove from Database]
    --> [Search Carrier] --> [Display Results]
```

### Sequence Diagram:
```plaintext
User --> Frame: Click Add/Delete/Search
Frame --> Database: Perform Add/Delete/Search
Database --> Frame: Return Results
Frame --> User: Display Updated Grid
```

### Code Snippets:
```delphi
procedure TFRAMEwarehouseCarrier.ACTaddExecute(Sender: TObject);
begin
  // Add carrier logic here
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

1. **Initialization**:
   - The `Create` constructor sets up the frame with default settings.
2. **Hidden Fields**:
   - Certain fields (e.g., `warehouseCode`, `updBy`, `lastUpd`) are hidden from the grid.

---

## 12. Conclusion:

The `FRwarehouseCarrier` code unit provides a robust framework for managing warehouse carriers. It leverages DevExpress components for an advanced grid interface and integrates with SOAP services for backend operations. However, the code lacks explicit error handling and validation, which could be improved for better reliability.

---

## 13. Short Summary:

The `FRwarehouseCarrier` unit implements a grid-based interface for managing warehouse carriers, supporting add, delete, and search functionalities. It integrates with SOAP services and uses DevExpress components for UI. The code is functional but lacks explicit error handling and validation.#### **FRwarehouseCarrier.pas**

```
unit FRwarehouseCarrier;

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
  TFRAMEwarehouseCarrier = class(TFRAMEBaseGridEditSOA)
    cxEDTfindCarrierDetail: TcxEditRepositoryButtonItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
    procedure cxDBVtableFocusedItemChanged(Sender: TcxCustomGridTableView;
      APrevFocusedItem, AFocusedItem: TcxCustomGridTableItem);
  private
    { Private declarations }
    procedure m_FindCarrier(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeCarrier(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_SetOnSetAccessMode(Sender: TObject; var pv_State: Boolean);
    function m_FindByCodeCarriers(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem): Boolean;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMEwarehouseCarrier: TFRAMEwarehouseCarrier;

implementation

uses
  kneFindDialogSOA, kneUtils, kneInterfaces, Global,
  CarrierServiceUtils, BaseServiceUtils, kneTypes,
  kneFindDialog, kneDialogFactory, kneFGFindUtils, kneFREditSOA;

{$R *.dfm}

constructor TFRAMEwarehouseCarrier.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'warehouseCode';
  DataPacketName := 'Carrier';                       // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'carriers';                        // nome do campo da metadata que vai conter os details
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
    HiddenFields.Clear;
    HiddenFields.Add('warehouseCode');
    HiddenFields.Add('updBy');
    HiddenFields.Add('lastUpd');
    // Ordem Campos ............................................................
    OrderFields.Add('code');
    OrderFields.Add('name');
    // Key Fields ..............................................................
    KeyFields:= 'warehouseCode;code';
    // Custom Editors ..........................................................
    AddCustomField('code','cxEDTfindCarrierDetail');
  end; //with

  // Atribui��o dos eventos dos Finds
  cxEDTfindCarrierDetail.Properties.OnButtonClick := m_FindCarrier;
  OnSetAccessMode := m_SetOnSetAccessMode;
end;

procedure TFRAMEwarehouseCarrier.m_SetOnSetAccessMode(Sender: TObject; var pv_State: Boolean);
begin
//  if (not assigned(CDStable)) or (not CDStable.Active) then  //JAR 10-02-2010  Aqui N�o � necess�ria a protec��o, pois n�o acede ao datset
//    exit;
  SetColsWidthInGrid('80;200;', cxDBVtable);
end;


procedure TFRAMEwarehouseCarrier.m_FindByCodeCarrier(
  Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem);
var
  lv_Service: TCarrierServiceUtils;
```

#### **FRwarehouseCarrier.dfm**

```
inherited FRAMEwarehouseCarrier: TFRAMEwarehouseCarrier
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTfindCarrierDetail: TcxEditRepositoryButtonItem
      Properties.Buttons = <
        item
          Default = True
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FF00FF4A667C
            BE9596FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FF6B9CC31E89E84B7AA3C89693FF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF4BB4FE51B5FF
            2089E94B7AA2C69592FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FF51B7FE51B3FF1D87E64E7AA0CA9792FF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            51B7FE4EB2FF1F89E64E7BA2B99497FF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF52B8FE4BB1FF2787D95F6A76FF
            00FFB0857FC09F94C09F96BC988EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FF55BDFFB5D6EDBF9D92BB9B8CE7DAC2FFFFE3FFFFE5FDFADAD8C3
            B3B58D85FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEA795FD
            EEBEFFFFD8FFFFDAFFFFDBFFFFE6FFFFFBEADDDCAE837FFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFC1A091FBDCA8FEF7D0FFFFDBFFFFE3FFFFF8FFFF
            FDFFFFFDC6A99CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC1A091FEE3ACF1
            C491FCF2CAFFFFDDFFFFE4FFFFF7FFFFF7FFFFE9EEE5CBB9948CFF00FFFF00FF
            FF00FFFF00FFFF00FFC2A191FFE6AEEEB581F7DCAEFEFDD8FFFFDFFFFFE3FFFF
            E4FFFFE0F3ECD2BB968EFF00FFFF00FFFF00FFFF00FFFF00FFBC978CFBE7B7F4
            C791F2C994F8E5B9FEFCD8FFFFDDFFFFDCFFFFE0E2D2BAB68E86FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFD9C3A9FFFEE5F7DCB8F2C994F5D4A5FAE8BDFDF4
            C9FDFBD6B69089FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB58D85E8
            DEDDFFFEF2F9D8A3F4C48CF9D49FFDEAB8D0B49FB89086FF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFAD827FC9AA9EEFE0B7EFDFB2E7CEACB890
            86B89086FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFBA968ABB988CB79188FF00FFFF00FFFF00FFFF00FF}
          Kind = bkGlyph
        end>
      Properties.CharCase = ecUpperCase
      Properties.ClickKey = 114
    end
  end
end
```
<!-- tabs:end -->

