<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa um componente de interface gráfica para gerenciar custos logísticos padrão associados a consignatários. Ele permite a visualização, edição e manipulação de dados relacionados a métodos de envio, armazéns e custos padrão. O objetivo principal é fornecer uma interface para que os usuários possam gerenciar esses dados de forma eficiente.

* **Tecnologias Utilizadas:**
  - **Delphi:** Linguagem de programação utilizada para criar a aplicação.
  - **Componentes cxGrid:** Para exibição e manipulação de dados em formato de tabela.
  - **SOAP:** Para comunicação com serviços externos.
  - **TClientDataSet:** Para manipulação de dados em memória.
  - **Bibliotecas de terceiros:** Como `kneUtils`, `Global`, `kneTypes`, entre outras.

* **Tipo de Interface:**
  - **Exibição em Grade (Grid Display):**
    - **Colunas da Grade e seus Tipos:**
      - `stat` (Status): Tipo string.
      - `whse` (Armazém): Tipo string.
      - `whseName` (Nome do Armazém): Tipo string.
      - `shipMthd` (Método de Envio): Tipo string.
      - `shipMthdName` (Nome do Método de Envio): Tipo string.
      - `payload` (Carga): Tipo numérico.
      - `stdCost` (Custo Padrão): Tipo numérico.
    - **Ações da Grade e seus Efeitos:**
      - Edição de valores diretamente na grade.
      - Adição e exclusão de registros.
      - Busca por métodos de envio e armazéns.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Adicionar novos registros.
  - Excluir registros existentes.
  - Editar valores diretamente na grade.
  - Buscar métodos de envio e armazéns por meio de botões de busca.

* **Componentes Principais:**
  - **Grade (cxGrid):** Exibe os dados em formato tabular.
  - **Botão "Loads":** Executa a ação associada à carga de dados.
  - **Painel de Ações:** Contém botões para adicionar, excluir e outras ações.

* **Tradução para Pseudo-código:**
  - Evento `OnEditValueChanged`: `se valor da célula for alterado então validar e salvar`.
  - Evento `OnNewRecord`: `se novo registro for criado então inicializar valores padrão`.
  - Ação `ACTloadsExecute`: `se botão "Loads" for clicado então carregar dados`.
  - Ação `ACTaddExecute`: `se botão "Adicionar" for clicado então criar novo registro`.
  - Ação `ACTdeleteExecute`: `se botão "Excluir" for clicado então remover registro selecionado`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do componente:
     - Configuração de propriedades como `MasterKeyFields`, `DataPacketName`, e `FrameType`.
     - Configuração da grade, incluindo campos ocultos, ordem de exibição e editores personalizados.
  2. Interação do usuário:
     - O usuário pode adicionar, editar ou excluir registros.
     - Botões de busca permitem localizar métodos de envio e armazéns.
  3. Funções executadas:
     - `m_FindByCodeShipMethod` (arquivo: `FRconsStandardLogisticCosts.pas`): Localiza método de envio por código.
     - `m_FindByCodeWhse` (arquivo: `FRconsStandardLogisticCosts.pas`): Localiza armazém por código.
     - `m_CalcLogCostsStat` (arquivo: `FRconsStandardLogisticCosts.pas`): Calcula estatísticas de custos logísticos.

* **Dados Necessários:**
  - Código do consignatário.
  - Código do armazém.
  - Método de envio.
  - Carga e custo padrão.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Botão "Adicionar": Habilitado sempre.
  - Botão "Excluir": Habilitado apenas se um registro estiver selecionado.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se um campo obrigatório estiver vazio.
  - "Valor inválido" se um valor não atender aos critérios de validação.

* **Valores Padrão dos Campos:**
  - Não definidos explicitamente no código.

* **Validações e Condições dos Campos:**
  - `whse` e `shipMthd`: Devem ser preenchidos.
  - `payload` e `stdCost`: Devem ser valores numéricos.

---

## 5. Funções Principais:

* **Descrição das Funções:**
  - `m_FindByCodeShipMethod`: Localiza método de envio por código.
  - `m_FindByCodeWhse`: Localiza armazém por código.
  - `m_CalcLogCostsStat`: Calcula estatísticas de custos logísticos.
  - `m_SetLoadsBtnStat`: Configura o estado do botão "Loads".

---

## 6. Consumo de Serviços API:

* **Chamadas a Serviços Externos:**
  - **Nome do Serviço:** `ConsigneeServiceUtils`.
  - **Endpoint:** Não especificado no código.
  - **Dados Enviados:** Não especificados no código.
  - **Dados Recebidos:** Não especificados no código.
  - **Propósito:** Gerenciar dados de consignatários.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`: Para exibição de dados em grade.
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.
  - `kneUtils`, `Global`, `kneTypes`: Utilitários personalizados.

* **Componentes Personalizados:**
  - `TFRAMEBaseGridEditSOA`: Classe base para o frame.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `stat` (string, obrigatório).
  - `whse` (string, obrigatório).
  - `whseName` (string, opcional).
  - `shipMthd` (string, obrigatório).
  - `shipMthdName` (string, opcional).
  - `payload` (numérico, obrigatório).
  - `stdCost` (numérico, obrigatório).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não especificado no código.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```pascal
  procedure TFRAMEconsStandardLogisticCosts.ACTloadsExecute(Sender: TObject);
  begin
    // Lógica para carregar dados
  end;
  ```
* **HTML Representando a Grade:**
  ```html
  <table style="width:100%; border:1px solid black;">
    <thead>
      <tr>
        <th>Status</th>
        <th>Armazém</th>
        <th>Nome do Armazém</th>
        <th>Método de Envio</th>
        <th>Nome do Método de Envio</th>
        <th>Carga</th>
        <th>Custo Padrão</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>Ativo</td>
        <td>WH001</td>
        <td>Armazém Central</td>
        <td>EXP</td>
        <td>Expresso</td>
        <td>1000</td>
        <td>500.00</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* **Configuração da Grade:**
  ```pascal
  DefineOrderFields('stat; whse; whseName; shipMthd; shipMthdName; payload; stdCost');
  ```

* **Atribuição de Eventos:**
  ```pascal
  cxEDTfind.Properties.OnButtonClick := m_FindWhse;
  ```

---

## 12. Conclusão:

O código fornece uma interface robusta para gerenciar custos logísticos padrão, com suporte a edição em grade e integração com serviços externos. No entanto, faltam detalhes sobre validações e endpoints de serviços, o que pode limitar sua aplicabilidade em cenários mais complexos.

---

## 13. Resumo Curto:

O código implementa um componente para gerenciar custos logísticos padrão, permitindo edição em grade, integração com serviços SOAP e busca de dados relacionados a métodos de envio e armazéns.#### **FRconsStandardLogisticCosts.pas**

```
unit FRconsStandardLogisticCosts;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel,kneFGFindUtils;

type
  TFRAMEconsStandardLogisticCosts = class(TFRAMEBaseGridEditSOA)
    cxEDTshipMthd: TcxEditRepositoryButtonItem;
    PNL1: TsPanel;
    BTNloads: TsBitBtn;
    ACTloads: TAction;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure CDStableNewRecord(DataSet: TDataSet);
    procedure ACTaddExecute(Sender: TObject);
    procedure CDStableAfterScroll(DataSet: TDataSet);
    procedure ACTloadsExecute(Sender: TObject);
    procedure ACTdeleteExecute(Sender: TObject);
    procedure CDStableAfterPost(DataSet: TDataSet);
  private
    { Private declarations }
    procedure m_FindByCodeShipMethod(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindByCodeWhse(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindServiceShipMethod(Sender: TObject;
      AButtonIndex: Integer);
    procedure m_FindWhse(Sender: TObject; AButtonIndex: Integer);
    procedure m_SetAccessMode(Sender: TObject; var lv_mode: Boolean);
    procedure m_CalcLogCostsStat(const pv_ConsCd: string);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    procedure m_SetLoadsBtnStat;
  end;

var
  FRAMEconsStandardLogisticCosts: TFRAMEconsStandardLogisticCosts;

implementation

uses
  kneUtils, Global, kneTypes,
  kneFindDialog, kneDialogFactory,
  //---
  ConsigneeServiceUtils {#22894},
  WarehouseNoDetailsServiceUtils, ShippingServiceUtils;

{$R *.dfm}

{ TFRAMEconsStandardLogisticCosts   //JAR #16317 #16325  2013-04-23   }

constructor TFRAMEconsStandardLogisticCosts.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'consCode=cons';
  DataPacketName := 'ConsStdCost';
  PropertyName := 'cStdCost';
  FrameType := frtDetail;

  ProviderService := TConsigneeServiceUtils.Create(Self);// [02-11-2016, #22894]
  
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
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('stat; whse; whseName; shipMthd; shipMthdName; payload; stdCost');
    // Key Fields ..............................................................
    KeyFields:= 'consCode;whse;shipMthd;payload';
    // Custom Editors ..........................................................
    AddCustomField('whse','cxEDTfind');
    AddCustomField('shipMthd','cxEDTshipMthd');
    AddCustomField('stat','cxEDTstat');
  end; //with
  ColsWidthInGrid := '100;60;220;80;160;100;100';

  // Atribui��o dos eventos dos Finds
  cxEDTfind.Properties.OnButtonClick := m_FindWhse;
  cxEDTshipMthd.Properties.OnButtonClick := m_FindServiceShipMethod;
```

#### **FRconsStandardLogisticCosts.dfm**

```
inherited FRAMEconsStandardLogisticCosts: TFRAMEconsStandardLogisticCosts
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
      OptionsData.Deleting = False
      OptionsData.Inserting = False
    end
  end
  inherited PNLfooter: TsPanel
    inherited PNLeditActions: TsPanel
      Width = 423
      object PNL1: TsPanel
        Left = 321
        Top = 1
        Width = 80
        Height = 30
        Align = alLeft
        TabOrder = 4
        SkinData.SkinSection = 'ALPHACOMBOBOX'
        object BTNloads: TsBitBtn
          Left = 3
          Top = 3
          Width = 70
          Height = 24
          Action = ACTloads
          Caption = 'Loads'
          TabOrder = 0
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00D0D0D000CECECE00CECECE00CECECE00CECECE00CECECE00CECECE00CECE
            CE00CECECE00CECECE00CECECE00D0D0D000FF00FF00FF00FF00FF00FF009C9C
            9C00737373006F6F6F006F6F6F006F6F6F006F6F6F006F6F6F006F6F6F006F6F
            6F006F6F6F006F6F6F006F6F6F00737373009C9C9C00FF00FF00D28E4B00CB6E
            1800BE580000BA540000B6500000B24C0000AF490000AD470000AA440000A842
            0000A7410000A6410200AA470900A9601E0073737300D0D0D000E9BC9000DB9D
            6500D38E5500D48F5400D28D5200D28B5000D38B4D00D1884A00D0864600D185
            4200C0824400A4875500BF6A2900A94504006F6F6F00CECECE00EDC9A500D8A7
            8500F6E4D200EFCEAD00DEB39500F5E2CF00EDC9A500EACCB300F5E1CD00EBC3
            9C00A9997F000CCEFF00D0813B00A64000006F6F6F00CECECE00EECDAB00D59F
            7A00E6CBBB00E3BFA700D39F7D00E6CABA00E2BA9E00D5A38400E6CAB900E0B6
            9600CBA385004BDBFF00CF834100A74100006F6F6F00CECECE00EFD0B000D198
            7100E0B59600DCA77C00D39A7200DFB39300DBA47600D5A17D00DFB39200DAA1
            7000D4A4850090E9FF00CF854600A94300006F6F6F00CECECE00F0D3B500D9AB
            8D00FDF8F300F6E3D000D6A78B00FCF7F200F4E0CB00E2BFA900FCF6F000EED1
            B500CA906F00DEF8FF00D0874C00AB4500006F6F6F00CECECE00F1D5B800D59E
            7600C9885E00C9804B00C8855800C8855900C87E4800C8845700C8855800C87D
            4500A36B5E0052347600C98A5B00AD4700006F6F6F00CECECE00F2D6BA00E1BC
            A400FAEEE300F1D6BA00DFB79C00F9EDE200F0D1B100F6E4D100F9ECE000EECB
            A800B88A7F00365CFB009C6C6A00B14B00006F6F6F00CECECE00F2D7BC00DAAA
            8600EFCEAD00E8BD9600CF8C5800EECCAA00E7B99000CF8B5700EDC9A400E0AA
            7A00CE885200D5B39F00D29E7600B44E00006F6F6F00CECECE00F2D8BD00D98B
            3E00B7CA9300C3CD9B00CCCFA200CFD0A400CDCFA200C5CE9D00B9CB9400AAC7
            890099C27C0099A75300D88A3D00B85200006F6F6F00CECECE00F2D9BE00D785
            3400BFEFD000D2F4DD00DEF7E600E2F8EA00DFF7E700D4F4DF00C3F0D200AEEB
            C20095E5AF0091C07600D98C3F00BD5700006F6F6F00CECECE00F2D9BE00D786
            360065D4830080DB980086DB9A0087DB9A0084D995007CD68E0072D2850066CE
            7A0058CA6E005DA63A00D98D4100C25C000073737300D0D0D000F2D9BE00EFD0
            B000C3BA8000B8BD8000B6BA7B00B5B77500B3B46F00B1B06900B0AD6400AEA9
            5E00ADA65800C39A4D00E2A86E00CC6A0D009C9C9C00FF00FF00DB9B5B00EFCF
            AE00F2D9BE00F2D8BE00F2D8BD00F2D7BC00F2D6BA00F1D5B800F0D3B500F0D1
            B100EFCEAD00EECBA700EABE9300D28F4B00FF00FF00FF00FF00}
          SkinData.SkinSection = 'BUTTON'
          ImageIndex = 2
          Images = IMLeditActions
        end
      end
    end
  end
  inherited CDStable: TClientDataSet
    OnNewRecord = CDStableNewRecord
  end
  inherited ACLeditActions: TActionList
    object ACTloads: TAction
      Caption = 'Loads'
      ImageIndex = 6
      OnExecute = ACTloadsExecute
    end
  end
  inherited IMLeditActions: TImageList
    Bitmap = {
      494C010107000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
```
<!-- tabs:end -->

