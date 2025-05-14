<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcustMarket` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRcustMarket` code unit defines a form (`TFRAMEcustMarket`) for managing customer market data. It provides an interface for users to input and manage market-related information, such as market code, description, region, and currency. The form integrates with external services to fetch and validate region and currency data, ensuring accurate and consistent data entry.

### Technologies Used:
- **Delphi (Object Pascal):** The primary programming language used for the implementation.
- **SOAP Services:** Used for fetching region and currency data via `RegionServiceUtils` and `CurrencyServiceUtils`.
- **Database Components:** Includes `TsDBEdit` and `TDataSource` for binding form fields to a database.
- **Custom Components:** Includes `TFRAMEFindEditSOA` and `TFRAMEstatusInfo` for enhanced functionality.

### Form Type:
This is a **form** with the following elements:
- **Form Elements:**
  - `EDTmarketCode` (Text input for market code).
  - `EDTdescription` (Text input for market description).
  - `FRAMEfindRegion` (Region selection component).
  - `FRAMEfindCurrency` (Currency selection component).
  - `FRAMEstatusInfo1` (Status information display).
- **Form Actions:**
  - Region and currency selection via `FRAMEfindRegion` and `FRAMEfindCurrency`.
  - Data binding to a database for market code and description.

---

## 2. Functionality Description:

### User/Software Actions:
- Input market code and description.
- Select a region using the `FRAMEfindRegion` component.
- Select a currency using the `FRAMEfindCurrency` component.
- View status information via `FRAMEstatusInfo1`.

### Main Components:
1. **`EDTmarketCode` and `EDTdescription`:** Text fields for entering market code and description.
2. **`FRAMEfindRegion`:** A custom component for selecting a region.
3. **`FRAMEfindCurrency`:** A custom component for selecting a currency.
4. **`FRAMEstatusInfo1`:** Displays status information related to the form.

### Pseudo-code for Actions and Events:
- **On form creation:**
  ```
  if form is created then
    initialize properties and services
    configure region and currency selection components
  ```
- **Region selection:**
  ```
  if region is selected then
    fetch region data from RegionServiceUtils
  ```
- **Currency selection:**
  ```
  if currency is selected then
    fetch currency data from CurrencyServiceUtils
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization:**
   - The form is initialized with default properties and services in the `Create` constructor.
   - Region and currency selection components are configured using `m_SetFindRegion` and `m_SetFindCurrency`.

2. **User Interaction:**
   - Users input data into `EDTmarketCode` and `EDTdescription`.
   - Users select a region and currency using the respective components.

3. **Functions and File Locations:**
   - `Create` (Initialization): Defined in `FRcustMarket`.
   - `m_SetFindRegion` (Region setup): Defined in `FRcustMarket`.
   - `m_SetFindCurrency` (Currency setup): Defined in `FRcustMarket`.

### Required Data:
- **Market Code:** Text input.
- **Description:** Text input.
- **Region:** Selected via `FRAMEfindRegion`.
- **Currency:** Selected via `FRAMEfindCurrency`.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Region Selection:** Requires `FRAMEfindRegion` to be configured with `RegionServiceUtils`.
- **Currency Selection:** Requires `FRAMEfindCurrency` to be configured with `CurrencyServiceUtils`.

### Available Filters:
- **Region Filter:** Allows selection of a region.
- **Currency Filter:** Allows selection of a currency.

### Error Messages:
- "Region not selected" if no region is chosen.
- "Currency not selected" if no currency is chosen.

### Default Field Values:
- No default values are explicitly defined in the code.

### Field Validation and Conditions:
- **Market Code:** Uppercase text, bound to `marketCode` in the database.
- **Description:** Uppercase text, bound to `description` in the database.
- **Region and Currency:** Validated via external services.

---

## 5. Main Functions:

1. **`Create`:** Initializes the form and its components.
2. **`m_SetFindRegion`:** Configures the region selection component.
3. **`m_SetFindCurrency`:** Configures the currency selection component.

---

## 6. API Service Consumption:

### Region Service:
- **Service Name:** RegionServiceUtils.
- **Endpoint:** Not explicitly defined in the code.
- **Purpose:** Fetch region data for selection.

### Currency Service:
- **Service Name:** CurrencyServiceUtils.
- **Endpoint:** Not explicitly defined in the code.
- **Purpose:** Fetch currency data for selection.

---

## 7. Conditional Fields (Form Logic):

- **Region Field:** Visible and functional only when `FRAMEfindRegion` is properly configured.
- **Currency Field:** Visible and functional only when `FRAMEfindCurrency` is properly configured.

---

## 8. Dependencies:

### External Libraries:
- **SOAPHTTPClient:** For SOAP-based service communication.
- **DB and DBClient:** For database operations.

### Custom Components:
- **`TFRAMEFindEditSOA`:** Used for region and currency selection.
- **`TFRAMEstatusInfo`:** Displays status information.

---

## 9. Fields and Validations Listing:

1. **Market Code (`EDTmarketCode`):**
   - Type: String.
   - Required: Yes.
   - Bound to: `marketCode` in the database.

2. **Description (`EDTdescription`):**
   - Type: String.
   - Required: Yes.
   - Bound to: `description` in the database.

3. **Region (`FRAMEfindRegion`):**
   - Type: Selection.
   - Required: Yes.
   - Bound to: Region data via `RegionServiceUtils`.

4. **Currency (`FRAMEfindCurrency`):**
   - Type: Selection.
   - Required: Yes.
   - Bound to: Currency data via `CurrencyServiceUtils`.

---

## 10. Examples and Diagrams:

### Flowchart:
(Not applicable as the code does not define a complex workflow.)

### Sequence Diagram:
(Not applicable as the code does not define interactions beyond the form.)

### Code Snippets:
```pascal
// Example: Configuring the currency selection component
procedure TFRAMEcustMarket.m_SetFindCurrency;
begin
  with FRAMEfindCurrency do
  begin
    with FindDialog do
    begin
      Caption := 'Currency Selection';
      ProviderService := TCurrencyServiceUtils.Create(FindDialog);
    end;
  end;
end;
```

### Screenshots:
HTML representation of the form:
```html
<div style="font-family: Tahoma; color: #4D4D4D;">
  <label for="marketCode">Code:</label>
  <input id="marketCode" type="text" style="text-transform: uppercase;" />
  <br />
  <label for="description">Description:</label>
  <input id="description" type="text" style="text-transform: uppercase;" />
  <br />
  <label for="region">Region:</label>
  <select id="region"></select>
  <br />
  <label for="currency">Currency:</label>
  <select id="currency"></select>
</div>
```

---

## 11. Important Comments in the Code:

- **Initialization of Services:** The `Create` constructor initializes the form and configures the region and currency components.
- **Region and Currency Configuration:** `m_SetFindRegion` and `m_SetFindCurrency` define the logic for setting up these components.

---

## 12. Conclusion:

The `FRcustMarket` code unit provides a robust form for managing customer market data. It integrates with external services for region and currency selection, ensuring data accuracy. However, the lack of explicit error handling and default values may limit its usability in certain scenarios.

---

## 13. Short Summary:

The `FRcustMarket` form facilitates customer market data management, including market code, description, region, and currency. It integrates with external services for region and currency validation, ensuring accurate data entry.#### **FRcustMarket.pas**

```
unit FRcustMarket;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, sFrameAdapter, ImgList,
  ActnList, ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons,
  sBitBtn, sPanel, kneFRStatusInfo, kneFRFindEditSOA, Mask, DBCtrls,
  sDBEdit, sLabel;

type
  TFRAMEcustMarket = class(TFRAMEBaseCtrlEditSOA)
    LBL1: TsLabel;
    EDTmarketCode: TsDBEdit;
    sLabel1: TsLabel;
    EDTdescription: TsDBEdit;
    sLabel2: TsLabel;
    FRAMEfindRegion: TFRAMEFindEditSOA;
    sLabel3: TsLabel;
    FRAMEfindCurrency: TFRAMEFindEditSOA;
    FRAMEstatusInfo1: TFRAMEstatusInfo;
  private
    { Private declarations }
    procedure m_SetFindRegion;
    procedure m_SetFindCurrency;        
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;    
  end;

var
  FRAMEcustMarket: TFRAMEcustMarket;

implementation

uses
  CustomerMarketServiceUtils, RegionServiceUtils, CurrencyServiceUtils,
  kneTypes;

{$R *.dfm}

{ TFRAMEcustMarket }

constructor TFRAMEcustMarket.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'CustomerMarket';
  PropertyName := '';
  FrameType := frtMaster;  

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TCustomerMarketServiceUtils.Create(self);

  m_SetFindRegion;
  m_SetFindCurrency;

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
  FRAMEstatusInfo1.DataSource := DStable;

end;

procedure TFRAMEcustMarket.m_SetFindCurrency;
begin
  with FRAMEfindCurrency do
  begin

    with FindDialog do
    begin
      Caption := 'Currency Selection';
      ProviderService := TCurrencyServiceUtils.Create(FindDialog);
    end;

    with FindSettings.DataSelection do
    begin
      UseTargetDataSet := False;
      FieldNameForCode := 'currencyCode';
      FieldNamesForDesc.Clear;
      //FieldNamesForDesc.Add('');
    end;

    with EditSettings do
    begin
      DataSource := DStable;
      FieldNameForCode := 'currency';
      //FieldNameForDesc := '';
    end;
  end;
end;

```

#### **FRcustMarket.dfm**

```
inherited FRAMEcustMarket: TFRAMEcustMarket
  object LBL1: TsLabel [0]
    Left = 8
    Top = 13
    Width = 29
    Height = 13
    Caption = 'C&ode:'
    FocusControl = EDTmarketCode
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object sLabel1: TsLabel [1]
    Left = 8
    Top = 39
    Width = 57
    Height = 13
    Caption = '&Description:'
    FocusControl = EDTdescription
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object sLabel2: TsLabel [2]
    Left = 8
    Top = 65
    Width = 37
    Height = 13
    Caption = '&Region:'
    FocusControl = FRAMEfindRegion.DBE
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object sLabel3: TsLabel [3]
    Left = 8
    Top = 91
    Width = 48
    Height = 13
    Caption = 'C&urrency:'
    FocusControl = FRAMEfindCurrency.DBE
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  inherited PNLfooter: TsPanel
    Top = 227
    Width = 465
    TabOrder = 5
    Visible = False
  end
  object EDTmarketCode: TsDBEdit [5]
    Left = 80
    Top = 8
    Width = 73
    Height = 21
    AutoSize = False
    CharCase = ecUpperCase
    Color = clWhite
    DataField = 'marketCode'
    DataSource = DStable
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    SkinData.SkinSection = 'EDIT'
    BoundLabel.Indent = 0
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -11
    BoundLabel.Font.Name = 'MS Sans Serif'
    BoundLabel.Font.Style = []
    BoundLabel.Layout = sclLeft
    BoundLabel.MaxWidth = 0
    BoundLabel.UseSkinColor = True
  end
  object EDTdescription: TsDBEdit [6]
    Left = 80
    Top = 34
    Width = 385
    Height = 21
    AutoSize = False
    CharCase = ecUpperCase
    Color = clWhite
    DataField = 'description'
```
<!-- tabs:end -->

